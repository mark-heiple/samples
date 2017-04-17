source("data.r")
source("functions.r")
source("score.r")

library(caret)
library(gbm)

#use caret package: requires a matrix of X variables, not a data frame of XY variables
data_matrix( ~., expandFactors = FALSE)
trim_matrix( ~.
             -bin_hinc-bin_tdon-bin_tlag
)
xx.train = data_rm_dups(xx.train,"log_")
xx.valid = data_rm_dups(xx.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xx.train.y = data_rm_dups(xx.train.y,"log_")
xx.valid.y = data_rm_dups(xx.valid.y,"log_")

getModelInfo()$gbm$parameters

objControl <- trainControl(method='cv', 
                           number=10, 
                           summaryFunction=defaultSummary,         #RMSE?
                           returnResamp='none')

gbmGrid = expand.grid(interaction.depth = c(2, 3, 4),
                        n.trees = 500+(1:3)*250, 
                        shrinkage = seq(.02, .04,.002),
                        n.minobsinnode = c(3,5,10))   #notes say this tends to not be important

#### this runs for a long time #####
set.seed(1)
objModel <- train( xx.train.y, y.train,
                  method='gbm', 
                  trControl=objControl,
                  tuneGrid=gbmGrid,
                  metric = "RMSE",
                  maximize=FALSE)

#display variable ranking
summary(objModel)

#display the chosen tuning parameters
objModel

#note: train wants a matrix (xx.train.y) while gbm wants a data.frame (xy.train.y)
data_dframe(expandFactors = FALSE)

xy.train = data_rm_dups(xy.train,"log_")
xy.valid = data_rm_dups(xy.valid,"log_")
xx.test = data_rm_dups(xx.test,"log_")
xy.train.y = data_rm_dups(xy.train.y,"log_")
xy.valid.y = data_rm_dups(xy.valid.y,"log_")


set.seed(1)

#fit the model using tuned parameters
gbm.fit= gbm(damt ~. 
             , data=xy.train.y, distribution="gaussian",
             
             n.trees=750,
             interaction.depth=2,
             shrinkage=.028,
             n.minobsinnode=5,
             verbose=F)

gbm.pred=predict(gbm.fit,newdata=xy.valid.y,n.trees=750)

summary(gbm.fit)
gbm.fit

gbm.mean = mean((y.valid - gbm.pred)^2)
gbm.sd = sd((y.valid - gbm.pred)^2)/sqrt(n.valid.y)
gbm.mean
gbm.sd

plot.residuals(y=y.valid,yhat=gbm.pred)

#variable importance
plot(gbm.fit,i="log_rgif")
