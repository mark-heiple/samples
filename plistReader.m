//
//  plistReader.m
//

#import "plistReader.h"
#import "NSString+MHString.h"
#import "NSArray+Intersect.h"

@interface plistReader (Indirection)

- (NSInteger) resolveValue:(NSString*)toResolve forIndex:(NSInteger)index;

@end


@implementation plistReader

@synthesize data;

- (id) initWithPlistName:(NSString*)plistName {

    self = [super init];
    if ( self ) {
        
        if( nil != plistName ) {
            NSBundle *mainBundle = [NSBundle mainBundle];
            NSURL *lists = [mainBundle URLForResource:plistName withExtension:@"plist"];

            data = [[NSMutableDictionary alloc] initWithContentsOfURL:lists];
        } else {
            NSLog(@"plistReader called with nil plist name");
        }
    }
    return self;
}

- (id) initWithPlistName:(NSString*)plistName withAlternate:(NSString*)alternate {
    self = [super init];
    if ( self ) {
        NSBundle *mainBundle = [NSBundle mainBundle];

        NSURL *lists = [mainBundle URLForResource:plistName withExtension:@"plist"];
        data = [NSMutableDictionary dictionaryWithContentsOfURL:lists];
        
        if( nil == data ) {
            NSURL *lists = [mainBundle URLForResource:alternate withExtension:@"plist"];
            data = [NSMutableDictionary dictionaryWithContentsOfURL:lists];
        }
        
        [data retain];
    }
    return self;
}

- (id) initWithDictionary:(NSDictionary*)d {
    self = [super init];
    if( self ) {
        data = [[NSMutableDictionary alloc] initWithDictionary:d];
    }
    return self;
}

- (void) dealloc {
    [data release];
    [super dealloc];
}

- (id) objectForPath:(NSString*)path {
    NSArray *parts = [NSArray arrayWithString:path seperator:'/'];
    NSString *key;
    NSString *branch;
    NSInteger len = [parts count];
    
    //key value is last
    key = [parts objectAtIndex:len-1];
    
    //rebuild branch without key
    NSRange range = NSMakeRange(0, len-1);
    branch = [NSMutableString stringWithArray:parts usingRange:range withChar:'/'];
    
    id obj = [self objectForKey:key fromBranch:branch];
    return obj;
}

- (id) objectForKey:(NSString*)key fromBranch:(NSString*)branch {
    NSArray *branches = [NSArray arrayWithString:branch seperator:'/'];
    NSInteger i, count = [branches count];
    NSString *s = nil;
    NSDictionary *d = data;
    NSString *k;
    
    for( i=0; i<count; i++ ) {
        k = [branches objectAtIndex:i];
        d = [d objectForKey:k];
    }
    
    s = [d objectForKey:key];
    //[branches release]; //autorelease
    return s;
}

- (NSString*) stringForKey:(NSString*)key fromBranch:(NSString*)branch {
    NSString *s = [self objectForKey:key fromBranch:branch];
    return s;
}

- (NSNumber*) numberForKey:(NSString*)key fromBranch:(NSString*)branch {
    NSNumber *n = [self objectForKey:key fromBranch:branch];
    return n;
}

- (NSInteger) integerForKey:(NSString*)key fromBranch:(NSString*)branch {
    NSNumber *n = [self numberForKey:key fromBranch:branch];
    NSInteger val = [n integerValue];
    return val;
}

- (BOOL) boolForKey:(NSString*)key fromBranch:(NSString*)branch {
    NSString *s = [self stringForKey:key fromBranch:branch];
    BOOL val = [s boolValue];
    return val;
}

- (NSArray*) arrayForKey:(NSString*)key fromBranch:(NSString*)branch {
    NSArray *a = [self objectForKey:key fromBranch:branch];
    return a;
}

- (NSArray*) arrayForIndex:(NSInteger)index fromArray:(NSArray*)a {
    
    NSMutableArray *array = nil;
    if( nil != a ) {
        if( index < 0 ) 
            index = 0;
        if( index >= [a count] ) 
            index = [a count]-1;
        
        //array is 0 based, level is 1 based
        NSString *s = [a objectAtIndex:index];
        array = [NSMutableArray arrayWithArray:[NSArray arrayWithString:s seperator:',']];
        
        //now must check each element - it may contain multiple items separated by '/'.
        //if so, replace string with another array
        NSInteger i, count = [array count];
        for( i=0; i<count; i++ ) {
            NSString *r = [array objectAtIndex:i];
            NSArray *parts = [NSArray arrayWithString:r seperator:'/'];
            if( [parts count] > 1 ) {
                //replace string with array
                [array replaceObjectAtIndex:i withObject:parts];
            }
        }
    }
    return array;
}

- (NSArray*) arrayForIndex:(NSInteger)index forKey:(NSString*)key fromBranch:(NSString*)branch {

    NSArray *array = nil;
    NSArray *a = [self arrayForKey:key fromBranch:branch];
    array = [self arrayForIndex:index fromArray:a];
    return array;
}

//gets the index from the array
- (NSInteger) integerForIndex:(NSInteger)index fromArray:(NSArray*)array {
    
    NSInteger value = NSNotFound;
    
    if( nil != array ) {
        if( index >= [array count] ) {
            index = [array count] - 1;
        }
        if( index < 0 ) {
            index = 0;
        }
        value = [[array objectAtIndex:index] integerValue];
    }
    
    return value;
}

//branch/table specifies an NSArray of integers, index gets the integer at that position from the array
- (NSInteger) integerForIndex:(NSInteger)index fromTable:(NSString*)table fromBranch:(NSString*)branch {
    
    NSInteger value = NSNotFound;
    
    //allow for indirection
    id obj = [self objectForKey:table fromBranch:branch];

    //is it an array?
    if( [obj isKindOfClass:[NSArray class]] ) {
    
        value = [self integerForIndex:index fromArray:obj];
    }
    
    else if( [obj isKindOfClass:[NSString class]] ) {
        value = [self resolveValue:obj forIndex:index];
    }
    
    return value;
    
}

//resolve keys for indirection
//this can specifiy multiple types of data
//  branch/key names an alternate NSArray
//  branch/key.dictKey names a dictionary:
//      branch/key is the NSDictionary
//          .dictKey is the NSArray of NSInteger values
//  branch/key:parameter names an alternate NSDictionary.
//  branch/key specifies a dictionary with the following specific format:
//      list = an NSArray of parameter names.
//      level = an NSArray of values, each row is a comma delimited list of values, ordered the same as the parameter names in list.
//
//      :parameter names the parameter to get from the comma delimited row 
- (NSInteger) resolveValue:(NSString *)toResolve forIndex:(NSInteger)index {
    
    NSInteger value = NSNotFound;
    
    //if key starts with '*', it indicates indirection
    //the redirected value is fully specifies the key/value
    
    NSString *newPath;
    NSString *parameter;
    id obj;
    
    if( [toResolve hasPrefix:@"*"] ) {
        
        //get new key and branch from string
        NSString *newKeyBranch = [toResolve substringFromIndex:1];  //remove '*'
        
        //split off the path part from the type
        NSArray *parts;
        
        //an NSDictionary?
        parts = [NSArray arrayWithString:newKeyBranch seperator:'.'];
        if( [parts count] >= 2 ) {
            
            newPath = [parts objectAtIndex:0];
            parameter = [parts objectAtIndex:1];
            
            obj = [self objectForPath:newPath];
            if( [obj isKindOfClass:[NSDictionary class]] ) {
                NSDictionary *d = obj;
                obj = [d objectForKey:parameter];
                
                //obj must be an array
                if( [obj isKindOfClass:[NSArray class]] ) {
                    value = [self integerForIndex:index fromArray:obj];
                }
            }
            
        } else {
            //a parameter list?
            parts = [NSArray arrayWithString:newKeyBranch seperator:':'];
            if( [parts count] >= 2 ) {
                
                newPath = [parts objectAtIndex:0];
                parameter = [parts objectAtIndex:1];
                
                obj = [self objectForPath:newPath];
                if( [obj isKindOfClass:[NSDictionary class]] ) {
                    NSDictionary *d = obj;
                    
                    //get the list
                    NSArray *list = [d objectForKey:@"list"];
                    NSArray *level;
                    NSArray *array;
                    
                    //position of parameter in list
                    NSInteger pos = [list indexOfObject:parameter];
                    if( NSNotFound != pos ) {
                        
                        //get the array
                        level = [d objectForKey:@"level"];
                        
                        //get the comma delimited row
                        array = [self arrayForIndex:index fromArray:level];
                        
                        //get the value from the array
                        if( pos < [array count] ) {
                            value = [[array objectAtIndex:pos] integerValue];
                        }
                    }
                }
            } else {
                
                //it is an array
                obj = [self objectForPath:newKeyBranch];
                if( [obj isKindOfClass:[NSArray class]] ) {
                    value = [self integerForIndex:index fromArray:obj];
                }
            }
        }
    }
    
    return value;
}



@end
