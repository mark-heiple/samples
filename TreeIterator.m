//
//  TreeIterator.m
//  Character Utility
//
//  Created by Mark Heiple on 4/17/12.
//  Copyright (c) 2012 Mark Heiple. All rights reserved.
//

#import "TreeIterator.h"
#import "NSIndexPath+Iterating.h"

@implementation TreeIterator

+ (id) iteratorWithArray:(NSArray*)array withChildKeyPath:(NSString*)path {
    id obj = [[[TreeIterator alloc] initWithArray:array withChildKeyPath:path] autorelease];
    return obj;
}

- (id) initWithArray:(NSArray*)array withChildKeyPath:(NSString*)path {
    self = [super init];
    if( self ) {
        arrayOfNodes = [array retain];
        childKeyPath = [path retain];
        i = nil;
    }
    return self;
}

- (void) dealloc {
    [i release];
    [childKeyPath release];
    [arrayOfNodes release];
    [super dealloc];
}

- (id)startAtIndex:(NSInteger)pos {
    [i release];
    i = nil;
    id d = nil;
    
    if( [arrayOfNodes count] >= pos ) {
        i = [[NSIndexPath alloc] initWithIndex:pos];
        d = [self objectAtIndexPath:i];
    }
    return d;
}

- (id)descend {
    
    NSIndexPath *ii = nil;
    id d = nil;
    
    ii = [i descend];
    d = [self objectAtIndexPath:ii];
    
    [i release];
    i = [ii retain];

    return d;
}

- (id)ascend {
    NSIndexPath *ii = nil;
    id d = nil;
    
    ii = [i ascend];
    d = [self objectAtIndexPath:ii];
    
    [i release];
    i = [ii retain];
    
    return d;
}

- (id) first {
    id d = nil;   
    
    [i release];
    i = nil;
    
    if( [arrayOfNodes count] > 0 ) {
        i = [[NSIndexPath alloc] initWithIndex:0];
        d = [self objectAtIndexPath:i];
    }
    return d;
}

- (id) next {
    id d, found = nil;  
    NSArray *a;
    NSIndexPath *ii;
    
    //get the current object
    d = [self objectAtIndexPath:i];
    if( nil != d ) {
        
        //if it has children, descend
        //if( [d isKindOfClass:[NSTreeNode class]] ) {
        
        if( [d respondsToSelector:@selector(childNodes)] ) {
            a = [d childNodes];
        } else if( [d respondsToSelector:@selector(objectForKey:)] ) {
            a = [d objectForKey:childKeyPath];
        } else {
            a = nil;
        }
        if( nil != a  && [a count] > 0 ) {
            
            ii = [i descend];
            found = [self objectAtIndexPath:ii];
            
        } else {
            
            ii = i;
            //no children, ascend until we find a new node, or run out
            do {
                ii = [ii increment];
                d = [self objectAtIndexPath:ii];
                
                if( nil == d ) {
                    //no more, pop back up to parent
                    ii = [ii ascend];
                }
            } while( [ii length] > 0 && nil == d );
            found = d;
        }
        
        //save new position
        [i release];
        i = [ii retain];
    }    
    
    return found;
}

//only enumerate the current child
- (id)nextChild {
    id found = nil;  
    NSIndexPath *ii = nil;
    
    if( nil != i ) {
        //get the current object
        found = [self objectAtIndexPath:i];
        if( nil != found ) {
            
            ii = [i increment];
            found = [self objectAtIndexPath:ii];
            
            //save new position
            [i release];
            i = [ii retain];
        }    
    }
    
    return found;
}

//returns the current index
- (NSIndexPath*)index {
    return i;
}

- (void) setIndex:(NSIndexPath*)path {
    [i release];
    i = [path retain];
}

- (id)objectAtIndexPath:(NSIndexPath*)path {
    NSUInteger index, pos;
    NSUInteger length = [path length];
    
    id d = nil, found = nil;
    NSArray *a = arrayOfNodes;
    
    for( pos = 0; pos < length && a != nil; pos++ ) {
        index = [path indexAtPosition:pos];
        if( index < [a count] ) {
            d = [a objectAtIndex:index];
            if( nil != d ) {
                
                //if( [d isKindOfClass:[NSTreeNode class]] ) {
                if( [d respondsToSelector:@selector(childNodes)] ) {
                    a = [d childNodes];
                } else if( [d respondsToSelector:@selector(objectForKey:)] ) {
                    a = [d objectForKey:childKeyPath];
                } else {
                    a = nil;
                }
            } else {
                a = nil;
            }
        } else {
            d = nil;
            a = nil;
        }
    }
    
    //found it?
    if( pos == length ) {
        found = d;
    }
    return found;
}

- (id)parentAtIndexPath:(NSIndexPath*)path {
    
    id d = nil;
    NSIndexPath *ii = [path ascend];
    if( nil != ii ) {
        d = [self objectAtIndexPath:ii];
    }
    return d;
}

- (id)parent {
    
    id d = [self parentAtIndexPath:i];
    return d;
}

- (id)object {
    return [self objectAtIndexPath:i];
}


@end
