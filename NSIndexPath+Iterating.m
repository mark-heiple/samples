//
//  NSIndexPath+Iterating.m
//

#import "NSIndexPath+Iterating.h"

@implementation NSIndexPath (Iterating)

+ (NSIndexPath*) indexPathWithIndexPath:(NSIndexPath*)path {
    return [[[NSIndexPath alloc] initWithIndexPath:path] autorelease];
}


#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-designated-initializers"
- (NSIndexPath*) initWithIndexPath:(NSIndexPath*)path {
    
    [self release];
    NSUInteger length = [path length];
    NSUInteger *array = malloc( length * sizeof(NSUInteger));
    [path getIndexes:array];
    
    NSIndexPath *i = [[NSIndexPath alloc] initWithIndexes:array length:length];
    free( array );
    return i;
}
#pragma clang diagnostic pop

//descends down to next child
- (NSIndexPath*)descend {
    NSIndexPath *i;
    
    //get current path
    NSUInteger length = [self length];
    NSUInteger *indexes = malloc(sizeof(NSUInteger)*(length+1));
    [self getIndexes:indexes];
    
    //add one level
    indexes[length] = 0;
    length++;
    
    //create new index
    i = [NSIndexPath indexPathWithIndexes:indexes length:length];
    free( indexes );
    
    return i;
}

//pops up to parent
- (NSIndexPath*)ascend {
    NSIndexPath *i = nil;
    
    //get current path
    NSUInteger length = [self length];
    NSUInteger *indexes = malloc(sizeof(NSUInteger)*length);
    [self getIndexes:indexes];
    
    //remove a level
    length--;
    
    //create new index
    if( length > 0 ) {
        i = [NSIndexPath indexPathWithIndexes:indexes length:length];
    }
    free( indexes );
    
    return i;
}

- (NSIndexPath*)increment {
    NSIndexPath *i;
    
    //get current path
    NSUInteger length = [self length];
    NSUInteger *indexes = malloc(sizeof(NSUInteger)*length);
    [self getIndexes:indexes];
    
    //increment
    indexes[length-1]++;
    
    //create new index
    i = [NSIndexPath indexPathWithIndexes:indexes length:length];
    free( indexes );
    
    return i;
}

- (NSIndexPath*)decrement {
    NSIndexPath *i = nil;

    //get current path
    NSUInteger length = [self length];
    NSUInteger *indexes = malloc(sizeof(NSUInteger)*length);
    [self getIndexes:indexes];
    
    //decrement (index is unsigned)
    
    indexes[length-1]--;
    if( (NSInteger)indexes[length-1] < 0 ) {
        length--;
    }
    
    //create new index
    if( (NSInteger)length > 0 ) {
        i = [NSIndexPath indexPathWithIndexes:indexes length:length];
    }
    free( indexes );
    
    return i;
}

- (NSInteger)lastIndex {
    
    NSInteger index = NSNotFound, last;
    
    last = [self length]-1;
    if( last > 0 ) {
        index = [self indexAtPosition:last];
    }
    
    return index;
}

@end
