//
//  NSDictionary+DeepCopy.h
//

#import <Foundation/Foundation.h>

@interface NSMutableDictionary (combining)

- (NSMutableDictionary*) initWithDictionaries:(NSDictionary*)d,...;
+ (NSMutableDictionary*) dictionaryWithDictionaries:(NSDictionary*)d,...;

- (NSMutableDictionary*) initWithDictionary:(NSDictionary*)d andObjects:(NSArray*)objs forKeys:(NSArray*)keys;
+ (NSMutableDictionary*) dictionaryWithDictionary:(NSDictionary*)d andObjects:(NSArray*)objs forKeys:(NSArray*)keys;

@end

@interface NSMutableDictionary (DeepCopy)

- (NSMutableDictionary*)mutableCopyDeep;
+ (NSMutableDictionary*)mutableCopyDeep:(NSDictionary*)dictionary;

- (void) deepRelease;

@end

@interface NSDictionary (DeepCopy)

- (NSDictionary*)mutableCopyDeep;
+ (NSDictionary*)mutableCopyDeep:(NSDictionary*)dictionary;

@end

@interface NSDictionary (DeepCompare)

- (BOOL) deepCompare:(NSDictionary*)dictionary;
+ (BOOL) deepCompare:(NSDictionary*)d1 withDictionary:(NSDictionary*)d2;

- (NSDictionary*)deepDifferences:(NSDictionary*)dictionary;
+ (NSDictionary*)deepDifferences:(NSDictionary*)d1 withDictionary:(NSDictionary*)d2;


@end
