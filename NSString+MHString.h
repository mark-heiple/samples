//
//  NSString+MHString.h
//

#import <Foundation/Foundation.h>

@interface NSString (MHString)

+ (NSString*) appendPrefix:(NSString*)prefix toString:(NSString*)string;

@end

@interface NSMutableString (MHString)


+ (NSMutableString*) stringWithFormat:(NSString*)format arguments:(NSArray*)args;

+ (NSMutableString*) stringWithArray:(NSArray*)parts withChar:(unichar)c;
- (NSMutableString*) initWithArray:(NSArray*)parts withChar:(unichar)c;

+ (NSMutableString*) stringWithArray:(NSArray*)parts withString:(NSString*)s;
- (NSMutableString*) initWithArray:(NSArray*)parts withString:(NSString*)s;

+ (NSMutableString*) stringWithArray:(NSArray*)parts usingRange:(NSRange)range withChar:(unichar)c;
- (NSMutableString*) initWithArray:(NSArray*)parts usingRange:(NSRange)range withChar:(unichar)c;

- (void)appendChar:(unichar)c;

- (NSInteger)formattedIntegerValue;
+ (NSInteger)formattedIntegerValueOfString:s;


//width specifiers don't work with %@

- (void)padToLength:(NSInteger)len;

+ (NSMutableString*)stringToNil:(NSString*)s;
+ (NSMutableString*)nilToString:(NSString*)s;

@end
