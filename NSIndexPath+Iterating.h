//
//  NSIndexPath+Iterating.h
//

#import <Foundation/Foundation.h>

@interface NSIndexPath (Iterating)

+ (NSIndexPath*) indexPathWithIndexPath:(NSIndexPath*)path;
- (NSIndexPath*) initWithIndexPath:(NSIndexPath*)path;

//descends down to next child
- (NSIndexPath*)descend;

//pops up to parent
- (NSIndexPath*)ascend;

//increment lowest child
- (NSIndexPath*)increment;

//decrement lowest child, if < 0 then ascend to parent
- (NSIndexPath*)decrement;

- (NSInteger)lastIndex;

@end
