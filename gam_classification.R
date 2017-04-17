source("data.r")
source("functions.r")
source("score.r")
library(gam)
library(pROC)
library(car)

#expand and standarize - expanding factors improves the fit
#data_dframe(expand=TRUE)
data_dframe(expand=FALSE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")

#candidate variables to find non linear functions
variables = 
c(
  #"hinc",
  "chld",
  #"wrat",
  "tdon",
  "tlag",
  "log_incm",
  "log_tgif"
)

#fit1 <- gam( donr ~ reg1+reg2+home+chld+wrat+bin_hinc+bin_tdon+bin_tlag+log_incm+log_tgif+I(hinc^2), 
#             data=xy.train, family=binomial("logit"))

allvariables =
  c(
    "reg1",
    "reg2",
    "home",
    "bin_hinc",
    "bin_tdon",
    "bin_tlag",
    "I(hinc^2)",
    "chld",
    "wrat",
    #"tdon",
    #"tlag",
    "log_incm",
    "log_tgif"
  )

#build a forumula using all variables in allvariables except for var
build_formula = function(var, other=allvariables) {
  
  f = ""
  
  for( v in other) {
    if(f==""){
      s = ""
    }else{
      s = " + "
    }
    if( v != var) {
      f = paste(f,v,sep=s)
    }
  }

  return(f)  
}

#function for using cross validation on a specific variable to test what degree to use
gam.cv = function( var, func, range ) {

  k = 10
  mean.profit = rep(0,max(range))  

  set.seed(1)
  folds = sample(1:k,nrow(xy.train), replace=TRUE)

  formulas = list()
  
  for( j in range ) {

    #build formula for gam
    ff = sprintf( "%s(%s,%d)", func, var, j)
    #print(ff)
    o = build_formula(var)
    #print(o)
    f = sprintf("donr ~ %s + %s", o, ff)
    #printf(f)
    asf = as.formula(f)
    
    #profits for this fold
    profit = rep(0,k)
    
    for( i in 1:k) {

      #this can quit with an error - getting Na/NaN/Inf in foreign function call errors
      #doesn't like the formula for some reason
      tryCatch ( 
        {
          cv.fit <- gam( asf, data=xy.train[folds!=i,], family=binomial("logit"))
          #summary(cv.fit)
          
          cv.pred = predict(cv.fit,newdata=xy.train[folds==i,],type="response")
          p = score_class_valid( c.train[folds==i], cv.pred, plot=FALSE )
          profit[i] = p$profit.max[2]
        },
        silent=TRUE,
        
      error = function(error) {
          profit[i] = 0
        },
      
      condition = function(cond) {
        },
      
      finally = function(cond) {
        }
      )
    }
    
    formulas[[j]] = ff
    mean.profit[j] = mean(profit)
  }
  
  best = which.max(mean.profit)
  form = formulas[[best]]
  
  printf("%s: profit = %f", form, mean.profit[best])
  
  return(c(form,mean.profit[best]))
}

#use the above cross validation function to look for the best spline function
best_function = function(var) {

  f = list()
  max = 4
  
  #use cross validation to find the best function for each
  #should I just pick from here also?
  
  i = 1
  f[[i]] = gam.cv(var,"poly",1:4)
  i = i+1
  f[[i]] = gam.cv(var,"ns",3:10)
  i = i+1
  f[[i]] = gam.cv(var,"bs",3:10)
  i = i+1
  f[[i]] = gam.cv(var,"s",2:10)
  
  extract = function(z,n) {
    return(z[n])
  }
  
  funcs = sapply(f, function(z) extract(z,1))
  profits = as.numeric(sapply(f, function(z) extract(z,2)))

  best = which.max(profits)  
  form = funcs[best]
  printf("%s: %s: profit = %f", var, form, profits[best])
  return(form)
}

## loop through all variables generating best spline function for each
## build the final best formula
test_all = function() {

  others = allvariables
  ff = ""
  
  for( var in variables ) {
    
    #get best spline formula
    f = best_function(var)
    ff = paste(ff,f,sep=" + ")
    
    #remove from global list
    others = others[others!=var]
  }
  
  o = build_formula("notinlist", others)
  form = sprintf( "donr ~ %s%s", o, ff)
  return(form)
}

#find the best formula using the different splines
#gam.formula = test_all()
gam.formula = "donr ~ reg1 + reg2 + home + bin_hinc + bin_tdon + bin_tlag + I(hinc^2) + wrat + bs(chld,3) + s(tdon,6) + s(tlag,2) + s(log_incm,3) + s(log_tgif,2)"

#fit chosen formula to full training set and validate
gam.fit <- gam( as.formula(gam.formula), data=xy.train, family=binomial("logit"))
summary(gam.fit)
gam.pred <- predict(gam.fit, xy.valid, type="response") # n.valid post probs
profit= score_class_valid( c.valid, gam.pred )
#vif(gam.fit)
gam.profit = profit$profit.max
gam.profit
profit

#wrat caused problems
#profit = 11909 /wout wrat /w hardcoded I(hinc^2)

#diagnostic plots
par(mfrow=c(1,3))
o = par(pty="s")
gam.roc <-  roc(response = c.valid, predictor=gam.pred)
plot(gam.roc, col="red")
par(o)
auc(gam.roc)
gam.lift = mhlift(c.valid,gam.pred)
plot.mhlift(gam.lift,col="red")
gam.cal = mhcalibration(c.valid,gam.pred)
plot.mhcalibration(gam.cal,col="red")
par(mfrow=c(1,1))

##### model with expanded factors is slightly better ######
#1252 / 11909
gam.fit
gam.profit
