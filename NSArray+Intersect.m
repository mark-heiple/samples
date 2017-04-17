//
//  NSArray+Intersect.m
//

#import "NSArray+Intersect.h"
#import "TreeIterator.h"

@implementation NSMutableArray (DeepCopy)

- (NSMutableArray*)mutableCopyDeep {
    
    return [NSMutableArray mutableCopyDeep:self];
}

+ (NSMutableArray*)mutableCopyDeep:(NSArray*)array {
    
    NSMutableArray *a = [[NSMutableArray alloc] initWithCapacity:[array count]];
    NSInteger i, count = [array count];
    
    for( i=0; i<count; i++ )  {
        
        id item = [array objectAtIndex:i];
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

        [a addObject:itemCopy];
        [itemCopy release];
    }
    return a;    
}

- (void) deepReleaseChildren {
    
    NSInteger i, count = [self count];
    
    for( i=count-1; i >= 0; i-- )  {
        
    	id item = [self objectAtIndex:i];
        
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

@implementation  NSArray (DeepCompare)

- (BOOL) deepCompare:(NSArray*)array {

    BOOL b = YES;
    
    b = [NSArray deepCompare:self withArray:array];
    
    return b;
}

//doesn't actually do a deep compare, just an unordered compare
+ (BOOL) deepCompare:(NSArray*)a1 withArray:(NSArray*)a2 {
    
    BOOL b = YES;
    
    NSInteger count1 = [a1 count];
    NSInteger count2 = [a2 count];
    
    if( count1 != count2 ) {
        b = NO;
    }
    
    else {
        
        NSArray *a = [NSMutableArray intersectArray:a1 withArray:a2];
        if( [a count] != [a1 count] ) {
            b = NO;
        }
    }
    
    return b;
}

- (NSArray*) deepDifferences:(NSArray*)array {
    
    NSArray *b;
    
    b = [NSArray deepDifferences:self withArray:array];
    
    return b;
}

//create a dictionary specifying the differences
+ (NSDictionary*)diffIndex:(NSInteger)i obj1:(id)obj1 obj2:(id)obj2 {
    
    NSNumber *index = [NSNumber numberWithInteger:i];
    NSArray *objs;
    NSArray *keys;
    
    if( nil != obj1 && nil != obj2 ) {
        //neither is nil, item is in both but different
        objs = [NSArray arrayWithObjects:index, obj1, obj2, nil];
        keys = [NSArray arrayWithObjects:@"_diffIndex", @"_diff1", @"_diff2", nil];
    }
    
    else if( nil == obj2 ) {
        //item is in obj1 but not obj2
        objs = [NSArray arrayWithObjects:index, obj1, nil];
        keys = [NSArray arrayWithObjects:@"_diffIndex", @"_diff1", nil];
    }
    
    else {
        //item is in obj2 but not obj1
        objs = [NSArray arrayWithObjects:index, obj2, nil];
        keys = [NSArray arrayWithObjects:@"_diffIndex", @"_diff2", nil];
    }
    
    NSDictionary *diff = [NSDictionary dictionaryWithObjects: objs
                                                     forKeys: keys
                          ];
    
    return diff;
}


//order matters
+ (NSArray*) deepDifferences:(NSArray*)a1 withArray:(NSArray*)a2 {
    
    id b = nil;
   
    NSMutableArray *differences = [NSMutableArray array];
    
    NSInteger i;
    NSInteger count1 = [a1 count];
    NSInteger count2 = [a2 count];
    NSInteger both = MIN(count1,count2);
    NSInteger unique1 = count1-both;
    NSInteger unique2 = count2-both;
    
    
    for( i=0; i<both; i++ )  {
        
        id item1 = [a1 objectAtIndex:i];
        id item2 = [a2 objectAtIndex:i];
        
        if ([item1 respondsToSelector: @selector(deepDifferences:)] &&
            [item2 respondsToSelector: @selector(deepDifferences:)] ) {
            
            b = [item1 deepDifferences:item2];
            if( nil != b ) {
                [differences addObject:b];
            }
        }
        
        else {
            if( ![item1 isEqual:item2] ) {
                [differences addObject:[NSArray diffIndex:i obj1:item1 obj2:item2]];
            }
        }
    }
    
    //add the items unique to each
    [differences addObjectsFromArray:[a1 subarrayWithRange:NSMakeRange(both, unique1)]];
    [differences addObjectsFromArray:[a1 subarrayWithRange:NSMakeRange(both, unique2)]];

    //return nil if no differences
    if( [differences count] == 0 ) {
        differences = nil;
    }
    return differences;
}

@end

@implementation NSArray (Intersect)


//intersect two arrays - all items must exist in both
//if one of the arrays is nil, then all items from the other are included
+ (NSMutableArray*) intersectArray:(NSArray*)array1 withArray:(NSArray*)array2
{
    NSMutableArray *array3 = nil;
    NSInteger i, count1 = [array1 count];
    NSString *s1;
    
    if( array1 == nil ) {
        array3 = [[NSMutableArray alloc] initWithArray:array2 copyItems:YES];
    }
    else if( array2 == nil ) {
        array3 = [[NSMutableArray alloc] initWithArray:array1 copyItems:YES];
    }
    else {
        array3 = [[NSMutableArray alloc] init];
        
        //find all objects in array1 that are also in array2 
        for( i=0; i<count1; i++ ) {
            s1 = [array1 objectAtIndex:i];
            if( [array2 indexOfObject:s1] != NSNotFound ) {
                [array3 addObject:s1];
            }
        }
    }
    
    return [array3 autorelease];
}

+ (NSMutableArray*) combineArray:(NSArray*)array1 withArray:(NSArray*)array2 {
    NSMutableArray *array3 = nil;
    NSInteger i;
    id obj;
    
    if( array1 == nil ) {
        array3 = [[NSMutableArray alloc] initWithArray:array2 copyItems:YES];
    }
    else if( array2 == nil ) {
        array3 = [[NSMutableArray alloc] initWithArray:array1 copyItems:YES];
    }
    else {
        array3 = [[NSMutableArray alloc] init];

        for( i=0; i<[array1 count]; i++ ) {
            obj = [array1 objectAtIndex:i];
            if( [array3 indexOfObject:obj] == NSNotFound ) {
                [array3 addObject:obj];
            }
        }
        
        for( i=0; i<[array2 count]; i++ ) {
            obj = [array2 objectAtIndex:i];
            if( [array3 indexOfObject:obj] == NSNotFound ) {
                [array3 addObject:obj];
            }
        }
    }
    
    return [array3 autorelease];
}

//performs array2 - array1
+ (NSMutableArray*) subtractArray:(NSArray*)array1 fromArray:(NSArray*)array2 {

    NSMutableArray *array3 = nil;
    
    array3 = [[NSMutableArray alloc] initWithArray:array2];
    
    NSInteger ii, i, count = [array1 count];
    for( i=0; i<count; i++ ) {
        ii = [array3 indexOfObject:[array1 objectAtIndex:i]];
        if( ii != NSNotFound ) {
            [array3 removeObjectAtIndex:ii];
        }
    }
    
    return [array3 autorelease];
}

+ (NSMutableArray*)arrayWithString:(NSString*)s seperator:(unichar)c {
    
    NSMutableArray *parts = [[NSArray alloc] initWithString:s seperator:c];
    return [parts autorelease];
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-designated-initializers"
- (NSMutableArray*)initWithString:(NSString*)s seperator:(unichar)c {
    
    [self release];
    
    NSMutableArray *parts = [[NSMutableArray alloc] init];
    NSString *token = [NSString stringWithFormat:@"%c", c];
    NSString *part;
    NSRange subRange, foundRange, searchRange = NSMakeRange(0, [s length]);
    
    do {
        foundRange = [s rangeOfString:token options:NSCaseInsensitiveSearch range:searchRange];
        
        if( foundRange.length > 0 ) {
            subRange = NSMakeRange(searchRange.location, foundRange.location - searchRange.location);
            part = [NSString stringWithString:[s substringWithRange:subRange]];
            [parts addObject:part];
        } else {
            //end of search, add this final token
            part = [NSString stringWithString:[s substringWithRange:searchRange]];
            if( [part length] > 0 ) {
                [parts addObject:part];
            }
            break;
        }
        searchRange = NSMakeRange( foundRange.location+1, [s length]-foundRange.location-1);
    } while( searchRange.length > 0 );
    
    return parts;
}

#pragma clang diagnostic pop


@end



@interface NSArray (MBIndexPathPrivate)

- (NSUInteger *)createIndexesOfPathToObject:(id)object count:(NSUInteger *)count;	//  Returns a dynamically allocated array which must be freed by the caller. *count must be 0 when passed in.
@end


@implementation NSArray (MBIndexPath)

- (id)objectAtIndexPath:(NSIndexPath *)indexPath;
{
	if (indexPath == nil)
        //[NSException raise:NSInvalidArgumentException format:nil];
		[NSException raise:NSInvalidArgumentException format:@""];
	
	id object = self;
	NSUInteger i;
	for (i = 0; i < [indexPath length]; i++)
	{
		if ([object isKindOfClass:[NSArray class]] == NO || [object count] <= [indexPath indexAtPosition:i])
            //[NSException raise:NSRangeException format:nil];
			[NSException raise:NSRangeException format:@""];
		object = [object objectAtIndex:[indexPath indexAtPosition:i]];
	}
	
	return object;
}

- (NSIndexPath *)indexPathOfObject:(id)object;
{
	if (object == nil)
        //[NSException raise:NSInvalidArgumentException format:nil];
		[NSException raise:NSInvalidArgumentException format:@""];
    
	NSUInteger count = 0;
	NSUInteger *indexes = [self createIndexesOfPathToObject:object count:&count];
	
	if (indexes == NULL)
		return nil;
    
	NSIndexPath *indexPath = [NSIndexPath indexPathWithIndexes:indexes length:count];
	free(indexes);
	return indexPath;
}

//  NSArray (MBIndexPathPrivate)
- (NSUInteger *)createIndexesOfPathToObject:(id)object count:(NSUInteger *)count;
{
	if (*count == NSUIntegerMax)
		return NULL;
	(*count)++;
	
	NSUInteger i;
	for (i = 0; i < [self count]; i++)
	{
		if ([[self objectAtIndex:i] isEqual:object])
		{
			NSUInteger *indexes = malloc(*count * sizeof(NSUInteger));
			if (indexes == NULL)
			{
				*count = NSUIntegerMax;
				return NULL;
			}
			indexes[*count - 1] = i;
			return (indexes + *count - 1);
		}
		else if ([[self objectAtIndex:i] isKindOfClass:[NSArray class]])
		{
			NSUInteger *indexes = [[self objectAtIndex:i] createIndexesOfPathToObject:object count:count];
			if (*count == NSUIntegerMax)
				return NULL;
			if (indexes != NULL)
			{
				*(indexes - 1) = i;
				return (indexes - 1);
			}
		}
	}
	
	(*count)--;
	return NULL;
}

@end

@implementation NSMutableArray (IndexSet)

+ (NSMutableArray*)arrayWithIndexSet:(NSIndexSet*)indexSet {
    
    NSMutableArray *array = [[NSMutableArray alloc] initWithIndexSet:indexSet];
    return [array autorelease];
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wobjc-designated-initializers"
- (NSMutableArray*)initWithIndexSet:(NSIndexSet*)indexSet {

    [self release];
    
    NSMutableArray *array = [[NSMutableArray alloc] init];
    
    NSInteger i;
    NSNumber *n;
    
    i = [indexSet firstIndex];
    while( NSNotFound != i ) {
        n = [NSNumber numberWithInteger:i];
        [array addObject:n];
        i = [indexSet indexGreaterThanIndex:i];
    }
    
    return array;
}
#pragma clang diagnostic pop

@end

@implementation NSMutableArray (Stack)

- (void) addObjectToTop:(id)anObject {
    
    [self insertObject:anObject atIndex:0];
}

- (void) addObjectToBottom:(id)anObject {
    
    [self addObject:anObject];
}

@end


@implementation NSMutableArray (Sorting)

/*
 Notes:
 
 for
 result = [left compare:right];
 
 result:
 NSOrderedAscending for left < right.
 NSOrderedDescendng for left > right.
*/
- (void) insertObject:(id<MHSorting>)left order:(NSInteger)order {

    id<MHSorting> right;
    NSInteger pos = -1, i, count = [self count];
    NSComparisonResult r;
    BOOL doInsert = YES;
    
    //find insert position
    for( i=0; i<count; i++ ) {
        right = [self objectAtIndex:i];
        
        r = [left compare:right];
        
        if( r == order ) {
            pos = i;
            break;
        }
        
        //combine if implemented
        if( r == NSOrderedSame ) {
            
            //right is the item from self
            SEL selCombine = @selector(combine:);
            if( [right respondsToSelector:selCombine] ) {
                
                [right performSelector:selCombine withObject:left];
                doInsert = NO;
                break;
            }
        }
    }
    
    if( doInsert ) {
        
        //get optional insert object
        id insertObj = left;
        SEL srcData = @selector(srcData);
        if( [insertObj respondsToSelector:srcData] ) {
            insertObj = [insertObj performSelector:srcData];
        }
        
        //insert the object
        if( pos < 0 ) {
            [self addObject:insertObj];
        } else {
            [self insertObject:insertObj atIndex:pos];
        }
    }
}

- (void) insertObjectAscending:(id<MHSorting>)obj {

    [self insertObject:obj order:NSOrderedAscending];
}

- (void) insertObjectDescending:(id<MHSorting>)obj {
    
    [self insertObject:obj order:NSOrderedDescending];
}

- (void) swapIndex:(NSInteger)i withIndex:(NSInteger)j {
    
    //must remove the higher index first
    //must add lower index first
    id<MHSorting> ii, jj;
    
    ii = [self objectAtIndex:i];
    jj = [self objectAtIndex:j];
    
    if( i > j ) {
        
        [self removeObjectAtIndex:i];
        [self removeObjectAtIndex:j];
        
        [self insertObject:ii atIndex:j];
        [self insertObject:jj atIndex:i];
        
    } else if( j > i ) {
        
        [self removeObjectAtIndex:j];
        [self removeObjectAtIndex:i];
        
        [self insertObject:jj atIndex:i];
        [self insertObject:ii atIndex:j];
    }
}

- (void) sort:(NSInteger)order {
    
    NSInteger i,j, count = [self count];
    id<MHSorting> left, right;
    bool swapped = YES;
    NSComparisonResult r;
    
    
    //bubble sort
    j = 0;

    while( swapped ) {
        
        swapped = NO;
        j++;
        
        for( i=0; i<count-j; i++ ) {
        
            left = [self objectAtIndex:i];
            right = [self objectAtIndex:i+1];
            
            r = [left compare:right];
            if( r != order && r != NSOrderedSame ) {
                
                //swap the items
                [self swapIndex:i withIndex:i+1];
                swapped = YES;
            }
            
            //combine if implemented
            if( r == NSOrderedSame ) {
                
                SEL selCombine = @selector(combine:);
                if( [left respondsToSelector:selCombine] ) {
                    
                    //combine them
                    [left performSelector:selCombine withObject:right];
                    
                    //remove right and fix indexes
                    [self removeObjectAtIndex:i+1];
                    i--;
                    count = [self count];
                    swapped = YES;
                }
            }
        }
    }
}


- (void) sortAscending {
    
    [self sort:NSOrderedAscending];
}

- (void) sortDescending {
    
    [self sort:NSOrderedDescending];
}

- (void) subtractMatchingItems:(NSArray*)array {
    
    NSInteger i, j, count = [array count];
    id<MHSorting> left, right;

    //for each item in array
    for( i = 0; i < count; i++ ) {
        
        //get item
        right = [array objectAtIndex:i];
        
        //compare with objects in this array
        for( j = [self count]-1; j >= 0; j-- ) {
            
            left = [self objectAtIndex:j];
            if( [left compare:right] == NSOrderedSame ) {
                
                //remove it
                [self removeObjectAtIndex:j];
                break;
            }
        }
    }
}

//sort the array to match the order of the keys
//the keyPath itentifies the item field to use for sort
//items that aren't in the list are at the bottom
- (void) sortByKeys:(NSArray*)keys keyPath:(NSString*)path {
    
    NSInteger i=0,j=0, pos=0, found = -1, count = [self count], kCount = [keys count];
    id obj;
    NSString *item, *key;
    
    for( i = 0; i < kCount; i++ ) {

        //sort key
        key = [keys objectAtIndex:i];
        found = -1;
        
        //find equivalent value in self
        for( j = pos; j<count; j++ ) {
            
            obj = [self objectAtIndex:j];
            if( [obj isKindOfClass:[NSDictionary class]] ) {
                item = [obj objectForKey:path];
            } else {
                item = obj;
            }
            
            if( [item compare:key options:NSCaseInsensitiveSearch] == NSOrderedSame ) {
                
                //found the item
                found = j;
                break;
            }
        }
        
        //pos is the position that items are being sorted to
        //found is the position that the item to be sorted was found
        if( found >= 0 ) {
            [self swapIndex:pos++ withIndex:found];
        }
    }
}

@end

@implementation SortedDictWrapper

@synthesize data;
@synthesize sortKey;

- (void) dealloc {
    
    [data release];
    [sortKey release];
    [super dealloc];
}

- (id) initWithData:(NSMutableDictionary*)d sortKey:(NSString*)key {
    
    self = [super init];
    if( self ) {
        data = [d retain];
        sortKey = [key retain];
    }
    return self;
}

+ (id) wrapperWithData:(NSMutableDictionary*)d sortKey:(NSString*)key {
    
    id obj = [[[SortedDictWrapper alloc] initWithData:d sortKey:key] autorelease];
    return obj;
}


//MSORTING
- (NSComparisonResult) compare:(NSMutableDictionary*)obj {
    
    NSComparisonResult r;
    NSString *value = [data objectForKey:sortKey];
    NSString *listValue = [obj objectForKey:sortKey];
    
    r = [value compare:listValue options:NSCaseInsensitiveSearch];
    return r;
}

- (NSMutableDictionary*)srcData {
    
    return data;
}


@end

@implementation NSArray (Query)

- (BOOL) compareObject:(id)obj1 with:(id)obj2 {

    BOOL b = NO;
    
    //classes must match
    if( [obj1 isKindOfClass:[obj2 class]] || [obj2 isKindOfClass:[obj1 class]] ) {
        
        if( [obj1 isKindOfClass:[NSString class]] ) {
            if( [obj1 compare:obj2 options:NSCaseInsensitiveSearch] == NSOrderedSame ) {
                b = YES;
            }
        }
        
        else {
            b = [obj1 isEqual:obj2];
        }
    }
    return b;
}


//selects the first array object from a flat array where specified key matches the value
- (id) selectWhere:(NSString*)key equals:(id)value {
    
    NSString *s;
    id d, found = nil;
    
    for( d in self ) {
    
        //requires MVC support?
        s = [d valueForKeyPath:key];
        if( [self compareObject:s with:value] ) {
            found = d;
            break;
        }
    }
    return found;
}

//selects the first array object from a tree where specified key matches the value, childPath specifies path to children
- (id) selectWhere:(NSString*)key equals:(id)value childPath:(NSString*)path {
    
    TreeIterator *ii = [TreeIterator iteratorWithArray:self withChildKeyPath:path];
    NSString *s;
    id d, found = nil;
    
    d = [ii first];
    while( d ) {
        
        //requires MVC support?
        s = [d valueForKeyPath:key];
        if( [self compareObject:s with:value] ) {
            found = d;
            break;
        }

        d = [ii next];
    }
    return found;
}

@end
             
@implementation NSMutableArray (Query)

//removes all items where key == value
- (void) removeWhere:(NSString*)key equals:(id)value {
    
    NSString *s;
    id d;
    NSInteger i, count = [self count];
    for( i = count-1; i >= 0; i-- ) {

        d = [self objectAtIndex:i];
        
        //requires MVC support?
        s = [d valueForKeyPath:key];
        if( [self compareObject:s with:value] ) {
            
            [self removeObject:d];
        }
    }
}


@end
             
