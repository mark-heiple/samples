//
//  NSArray+Intersect.h
//

#import <Foundation/Foundation.h>

@interface NSMutableArray (DeepCopy)

- (NSMutableArray*)mutableCopyDeep;
+ (NSMutableArray*)mutableCopyDeep:(NSArray*)array;
- (void) deepRelease;
@end

@interface NSArray (DeepCompare)

- (BOOL) deepCompare:(NSArray*)array;
+ (BOOL) deepCompare:(NSArray*)a1 withArray:(NSArray*)a2;

- (NSArray*) deepDifferences:(NSArray*)array;
+ (NSArray*) deepDifferences:(NSArray*)a1 withArray:(NSArray*)a2;

@end

@interface NSArray (Intersect)

+ (NSMutableArray*) intersectArray:(NSArray*)array1 withArray:(NSArray*)array2;
+ (NSMutableArray*) combineArray:(NSArray*)array1 withArray:(NSArray*)array2;
+ (NSMutableArray*) subtractArray:(NSArray*)array1 fromArray:(NSArray*)array2;

+ (NSMutableArray*)arrayWithString:(NSString*)s seperator:(unichar)c;
- (NSMutableArray*)initWithString:(NSString*)s seperator:(unichar)c;


@end


@interface NSArray (MBIndexPath)

- (id)objectAtIndexPath:(NSIndexPath *)indexPath;	//  Raises an NSRangeException if the indexPath goes beyond the bounds of the receiver.
- (NSIndexPath *)indexPathOfObject:(id)object;		//  Returns nil if the object does not exist within the receiver.

@end

@interface NSMutableArray (IndexSet)

+ (NSMutableArray*)arrayWithIndexSet:(NSIndexSet*)indexSet;
- (NSMutableArray*)initWithIndexSet:(NSIndexSet*)indexSet;

@end

@interface NSMutableArray (Stack)
- (void) addObjectToTop:(id)anObject;
- (void) addObjectToBottom:(id)anObject;
@end

@protocol MHSorting <NSObject>

@required
- (NSComparisonResult) compare:(id<MHSorting>)obj;

//optional for combining objects that are the same (used by insert)
@optional
- (void) combine:(id<MHSorting>)obj;

//if defined, this is the object that will be inserted instead of obj (see -insertObjectAscending/Descending)
- (id) srcData;

@end

@interface NSMutableArray (Sorting)

- (void) insertObjectAscending:(id<MHSorting>)obj;
- (void) insertObjectDescending:(id<MHSorting>)obj;
- (void) sortAscending;
- (void) sortDescending;
- (void) sortByKeys:(NSArray*)keys keyPath:(NSString*)path;

//array of id<MHSorting> items
- (void) subtractMatchingItems:(NSArray*)array;

@end

//this object is just a wrapper around a dictionary to allow sorting
@interface SortedDictWrapper : NSObject <MHSorting> {
    
    NSMutableDictionary *data;
    NSString *sortKey;
}

@property (readonly) NSMutableDictionary *data;
@property (readonly) NSString *sortKey;

- (id) initWithData:(NSMutableDictionary*)data sortKey:(NSString*)key;
+ (id) wrapperWithData:(NSMutableDictionary*)data sortKey:(NSString*)key;

//MSORTING
- (NSComparisonResult) compare:(NSMutableDictionary*)obj;
- (NSMutableDictionary*)srcData;

@end

@interface NSArray (Query)

//selects the first array object from a flat array where specified key matches the value
- (id) selectWhere:(NSString*)key equals:(id)value;

//selects the first array object from a tree where specified key matches the value, childPath specifies path to children
- (id) selectWhere:(NSString*)key equals:(id)value childPath:(NSString*)path;

@end

@interface NSMutableArray (Query)

//selects the first array object from a flat array where specified key matches the value
- (void) removeWhere:(NSString*)key equals:(id)value;

@end
