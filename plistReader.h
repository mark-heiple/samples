//
//  plistReader.h
//

#import <Foundation/Foundation.h>

@interface plistReader : NSObject {
    @protected

    NSMutableDictionary *data;
}

@property (readonly) NSDictionary* data;

- (id) initWithPlistName:(NSString*)plistName;
- (id) initWithPlistName:(NSString*)plistName withAlternate:(NSString*)alternate;
- (id) initWithDictionary:(NSDictionary*)d;

//path is branch/key
- (id) objectForPath:(NSString*)path;

- (id) objectForKey:(NSString*)key fromBranch:(NSString*)branch;
- (NSString*) stringForKey:(NSString*)key fromBranch:(NSString*)branch;
- (NSNumber*) numberForKey:(NSString*)key fromBranch:(NSString*)branch;
- (NSInteger) integerForKey:(NSString*)key fromBranch:(NSString*)branch;
- (BOOL) boolForKey:(NSString*)key fromBranch:(NSString*)branch;
- (NSArray*) arrayForKey:(NSString*)key fromBranch:(NSString*)branch;

- (NSArray*) arrayForIndex:(NSInteger)level forKey:(NSString*)key fromBranch:(NSString*)branch;

//for reading integers from an indexed table
//index is 0 based
- (NSInteger) integerForIndex:(NSInteger)index fromTable:(NSString*)table fromBranch:(NSString*)branch;


@end
