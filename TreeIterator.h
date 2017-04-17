//
//  TreeIterator.h
//  Character Utility
//
//  Created by Mark Heiple on 4/17/12.
//  Copyright (c) 2012 Mark Heiple. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface TreeIterator : NSObject {
    
    NSArray *arrayOfNodes;
    NSString *childKeyPath;
    
    //change to use an NSIndexPath
    NSIndexPath *i;
}

+ (id) iteratorWithArray:(NSArray*)array withChildKeyPath:(NSString*)path;
- (id) initWithArray:(NSArray*)array withChildKeyPath:(NSString*)path;

//for iterating through the array and children
- (id)startAtIndex:(NSInteger)pos;

//descend/ascend
- (id)descend;
- (id)ascend;

- (id)first;
- (id)next;
- (id)nextChild;    //only enumerate the current child

//warning! the returned index is NOT retained.
//all iteratating methods will release it (descent/ascend, first/next/nextChild)
- (NSIndexPath*)index;

- (void) setIndex:(NSIndexPath*)path;
- (id)objectAtIndexPath:(NSIndexPath*)path;
- (id)parentAtIndexPath:(NSIndexPath*)path;
- (id)parent;
- (id)object;


@end
