//
//  NSDictionary+DeepCopy.m
//

#import "NSDictionary+DeepCopy.h"
#import "NSArray+Intersect.h"

@implementation NSMutableDictionary (combining)

- (void) addDictionary:(NSDictionary*)d toDictionary:(NSMutableDictionary*)m {

    NSArray *keys = [d allKeys];
    for( NSString *s in keys ) {
        id obj = [d objectForKey:s];
        [m setObject:obj forKey:s];
    }
}

- (NSMutableDictionary*) initWithDictionaryList:(NSArray*)l {
    
    self = [super init];
    if( self ) {
        
        NSMutableDictionary *dd = [[NSMutableDictionary alloc] init];
        
        for( NSDictionary *d in l ) {

            [self addDictionary:d toDictionary:dd];
            //[self setDictionary:d]; //does this remove keys that aren't in d?
        }
        
        [self release];
        self = dd;
    }
    return self;
}

- (NSMutableDictionary*) initWithDictionaries:(NSDictionary*)i,...
{
    NSMutableArray *a = [[NSMutableArray alloc] init];
    NSDictionary *d;
    
    [a addObject:i];
    
    va_list args;
    va_start( args, i );
        
    
    d = va_arg( args, id );
    while( nil != d ) {

        [a addObject:d];
        d = va_arg( args, id );
    }

    //all done
    va_end( args );
    
    self = [self initWithDictionaryList:(NSArray*)a];
    [a release];
    
    return self;
}

+ (NSMutableDictionary*) dictionaryWithDictionaries:(NSDictionary*)i,...
{
    NSMutableArray *a = [[NSMutableArray alloc] init];
    NSDictionary *d;
    
    [a addObject:i];
    
    va_list args;
    va_start( args, i );
    
    d = va_arg( args, id );
    while( nil != d ) {
        
        [a addObject:d];
        d = va_arg( args, id );
    }
    
    //all done
    va_end( args );
    
    NSMutableDictionary *obj = [[[NSMutableDictionary alloc] initWithDictionaryList:a] autorelease];
    [a release];
    
    return obj;
}

- (NSMutableDictionary*) initWithDictionary:(NSDictionary*)d andObjects:(NSArray*)objs forKeys:(NSArray*)keys {

    self = [super init];
    if( self ) {
        
        NSMutableDictionary *dd = [[NSMutableDictionary alloc] initWithDictionary:d];
        
        id obj,key;
        NSInteger i, count = [objs count];
        for( i=0; i<count; i++ ) {
            obj = [objs objectAtIndex:i];
            key = [keys objectAtIndex:i];
            [dd setObject:obj forKey:key];
        }
        
        [self release];
        self = dd;
    }
    
    return self;
}

+ (NSMutableDictionary*) dictionaryWithDictionary:(NSDictionary*)d andObjects:(NSArray*)objs forKeys:(NSArray*)keys {
    
    NSMutableDictionary *obj = [[[NSMutableDictionary alloc] initWithDictionary:d andObjects:objs forKeys:keys] autorelease];
    return obj;
}

@end

@implementation NSMutableDictionary (DeepCopy)

- (NSMutableDictionary*)mutableCopyDeep {
 
    return [NSMutableDictionary mutableCopyDeep:self];
}

+ (NSMutableDictionary*)mutableCopyDeep:(NSDictionary*)dictionary {
    
    NSMutableDictionary *d = [[NSMutableDictionary alloc] initWithCapacity:[dictionary count]];
    NSArray *keys = [dictionary allKeys];
    
    for (id key in keys) {
        
    	id item = [dictionary valueForKey:key];
    	id itemCopy = nil;
        
        if ([item respondsToSelector: @selector(mutableCopyDeep)]) {
            itemCopy = [item mutableCopyDeep];
        }
    	else if ([item respondsToSelector:@selector(mutableCopyWithZone:)]) {
    		itemCopy = [item mutableCopy];
    	}
    	else {	
    		itemCopy = [item copy];
    	}
        
    	[d setValue:itemCopy forKey:key];
    	[itemCopy release];
    }
    return d;  
}

- (void) deepReleaseChildren {
    
    NSArray *keys = [self allKeys];
    id item;
    
    for (id key in keys) {
        
    	item = [self valueForKey:key];
        
        if ([item respondsToSelector: @selector(deepReleaseChildren)]) {
            [item deepReleaseChildren];
        }
    }
    
    [self removeAllObjects];
}


- (void) deepRelease {
    
    [self deepReleaseChildren];
    [self release];
}


@end


@implementation NSDictionary (DeepCopy)

- (NSDictionary*)mutableCopyDeep {
    
    return [NSDictionary mutableCopyDeep:self];
}

+ (NSDictionary*)mutableCopyDeep:(NSDictionary*)dictionary {

    NSDictionary *d = [NSMutableDictionary mutableCopyDeep:dictionary];
    return d;
}

@end

@implementation NSDictionary (DeepCompare)

- (NSDictionary*)deepDifferences:(NSDictionary*)dictionary {
    
    NSDictionary *d = [NSDictionary deepDifferences:self withDictionary:dictionary];
    return d;
}

//create a dictionary that explains the difference between the two objects
//should just be a scalar value (NSString or NSNumber)
+ (NSDictionary*)diffObj1:(id)obj1 obj2:(id)obj2 {
    
    NSArray *objs;
    NSArray *keys;
    
    if( nil != obj1 && nil != obj2 ) {
        //neither is nil, item is in both but different
        objs = [NSArray arrayWithObjects:obj1, obj2, nil];
        keys = [NSArray arrayWithObjects:@"_diff1", @"_diff2", nil];
    }
    
    else if( nil == obj2 ) {
        //item is in obj1 but not obj2
        objs = [NSArray arrayWithObjects:obj1, nil];
        keys = [NSArray arrayWithObjects:@"_diff1", nil];
    }
    
    else {
        //item is in obj2 but not obj1
        objs = [NSArray arrayWithObjects:obj2, nil];
        keys = [NSArray arrayWithObjects:@"_diff2", nil];
    }
    
    NSDictionary *diff = [NSDictionary dictionaryWithObjects: objs
                                                     forKeys: keys
                          ];
    
    return diff;
}

//create a difference of key values
+ (NSDictionary*)deepDifferences:(NSDictionary*)d1 withDictionary:(NSDictionary*)d2 {
    
    NSMutableDictionary *differences = [NSMutableDictionary dictionary];
    id b = nil;
    
    NSArray *keys1 = [d1 allKeys];
    NSArray *keys2 = [d2 allKeys];
    
    //get subset of identical keys
    NSArray *both = [NSArray intersectArray:keys1 withArray:keys2];

    //keys that are not common
    NSArray *unique1 = [NSArray subtractArray:both fromArray:keys1];
    NSArray *unique2 = [NSArray subtractArray:both fromArray:keys2];
    
    //check both
    for (id key in both) {
        
        id item1 = [d1 valueForKey:key];
        id item2 = [d2 valueForKey:key];
        
        if ([item1 respondsToSelector: @selector(deepDifferences:)] &&
            [item2 respondsToSelector: @selector(deepDifferences:)] ) {
            b = [item1 deepDifferences:item2];
            
            if( nil != b ) {
                [differences setObject:b forKey:key];
            }
        }
        else {
            
            //save the key that was different (don't care about the value)
            if( ![item1 isEqual:item2] ) {
                [differences setObject:[NSDictionary diffObj1:item1 obj2:item2] forKey:key];
            }
        }
    }
    
    //add unique keys
    NSString *k;
    id obj;
    
    if( [unique1 count] > 0 ) {
        for( k in unique1 ) {
            obj = [d1 objectForKey:k];
            [differences setObject:obj forKey:k];
        }
    }
    if( [unique2 count] > 0 ) {
        for( k in unique1 ) {
            obj = [d1 objectForKey:k];
            [differences setObject:obj forKey:k];
        }
    }
    
    if( 0 == [[differences allKeys] count] ) {
        differences = nil;
    }
    return differences;
}

- (BOOL) deepCompare:(NSDictionary*)dictionary {
    
    BOOL b = YES;
    
    b = [NSDictionary deepCompare:self withDictionary:dictionary];
    
    return b;
}
+ (BOOL) deepCompare:(NSDictionary*)d1 withDictionary:(NSDictionary*)d2 {
    
    BOOL b = YES;
    
    NSArray *keys1 = [d1 allKeys];
    NSArray *keys2 = [d2 allKeys];
    
    //are keys the same?
    b = [NSArray deepCompare: keys1 withArray:keys2];
    
    if( b ) {
    
        for (id key in keys1) {
            
            id item1 = [d1 valueForKey:key];
            id item2 = [d2 valueForKey:key];
            
            if ([item1 respondsToSelector: @selector(deepCompare:)] &&
                [item2 respondsToSelector: @selector(deepCompare:)] ) {
                b = [item1 deepCompare:item2];
            }
            else {	
                b = [item1 isEqual:item2];
            }
            
            if( !b ) {
                break;
            }
        }
    }    
    return b;
}


@end
