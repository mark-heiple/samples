//
//  NSString+MHString.m
//

#import "NSString+MHString.h"

@implementation NSString (MHString)

+ (NSString*) appendPrefix:(NSString*)prefix toString:(NSString*)string {
    
    NSString *s;
    
    //capitalize name
    //NSString *caps = [name capitalizedString];
    //just uppercase the first letter
    
    unichar first = [string characterAtIndex:0];
    first = toupper(first);
    
    //build string
    NSString *caps = [NSString stringWithFormat:@"%c%@", first, [string substringFromIndex:1]];
    
    //combine it
    s = [NSString stringWithFormat:@"%@%@",prefix, caps];
    return s;
}


@end


@implementation NSMutableString (MHString)

+ (id) testArgs:(NSString*)format arguments:(NSArray*)args, ...  {
    
    va_list list;
    
    va_start(list, args);
    
    
    id x;
    
    x = va_arg(list, id);

    while( nil != x ) {

        NSLog( @"arg = %@\n", x );
        
        x = va_arg(list, id);
    }
    
    
    va_end(list);
    
    return nil;
}

//to be consistent with sprintf, stringWithFormat, etc, the number of %@ characters is used
//to determine the number of arguments, not the length of args.  There can be extra args,
//but not fewer!
+ (NSMutableString*) stringWithFormat:(NSString*)format arguments:(NSArray*)args {
    
    NSInteger numArgs, i, count = [args count];
    
    NSObject **data = malloc( sizeof(NSObject*) * count );
    [args getObjects:data];

    
    //build up string one arg at a time
    NSMutableString *s = [NSMutableString stringWithString:format];
    NSRange r;
    
    //replace all '%@' items except for first with '@@'
    r = NSMakeRange( 0, [s length] );
    numArgs = [s replaceOccurrencesOfString:@"%@" withString:@"@@" options:NSCaseInsensitiveSearch range:r];

    //now replace '@@' with '%@' one at a time and replace characters
    for( i=0; i<numArgs && i < count; i++ ) {

        //find first '%@'
        r = [s rangeOfString:@"@@"];
        [s replaceOccurrencesOfString:@"@@" withString:@"%@" options:NSCaseInsensitiveSearch range:r];

        //debuging
        //NSLog( @"%@\n", s );
        //NSLog( @"arg %ld = %@\n", i, data[i] );
        
        //do string
        s = [NSMutableString stringWithFormat:s, data[i]];
    }
    
    //error debugging
    if( i< numArgs || i < count ) {
        if( i < numArgs ) {
            NSLog( @"-stringWithFormat:arguments: WARNING number of args < number of format specifiers\n" );
        }
        if( i < count ) {
            NSLog( @"-stringWithFormat:arguments: WARNING number of number of format specifiers < number of arguments\n" );
        }
        
        NSLog( @"format specifier: (%ld specifiers) %@\n", numArgs, format );
        NSLog( @"number of arguments: %ld\n", count );
    }
    free( data);
    
    return s;
}

- (void)appendChar:(unichar)c
{
    NSString *s = [NSString stringWithFormat:@"%c", c];
    [self appendString:s];
}

+ (NSMutableString*) stringWithArray:(NSArray*)parts withChar:(unichar)c
{
    NSMutableString *s = [[NSMutableString alloc] initWithArray:parts withChar:c];
    return [s autorelease];
}
- (NSMutableString*) initWithArray:(NSArray*)parts withChar:(unichar)c
{
    NSRange range = NSMakeRange(0, [parts count]);
    NSMutableString *s = [self initWithArray:parts usingRange:range withChar:c];
    return s;
}

+ (NSMutableString*) stringWithArray:(NSArray*)parts withString:(NSString*)sep {

    NSMutableString *s = [[NSMutableString alloc] initWithArray:parts withString:sep];
    return [s autorelease];
}
- (NSMutableString*) initWithArray:(NSArray*)parts withString:(NSString*)sep {
    
    //create string for property
    [self release];
    NSMutableString *s = [[NSMutableString alloc] init];
    NSString *part;
    
    //NSInteger i, last = range.location + range.length;   
    //for( i = range.location; i<last; i++ ) {

    NSInteger i, last = [parts count];
    for( i = 0; i<last; i++ ) {
    
        part = [parts objectAtIndex:i];
        [s appendString:part];
        if( i < last-1 ) {
            [s appendString:sep];
        }
    }
    
    self = s;
    return self;
}



+ (NSMutableString*) stringWithArray:(NSArray*)parts usingRange:(NSRange)range withChar:(unichar)c {
    NSMutableString *s = [[NSMutableString alloc] initWithArray:parts usingRange:range withChar:c];
    return [s autorelease];
}

- (NSMutableString*) initWithArray:(NSArray*)parts usingRange:(NSRange)range withChar:(unichar)c {
    
    //create string for property
    NSMutableString *s = [[NSMutableString alloc] init];
    NSString *part;
    
    NSInteger i, last = range.location + range.length;   
    for( i = range.location; i<last; i++ ) {
        part = [parts objectAtIndex:i];
        [s appendString:part];
        if( i < last-1 ) {
            [s appendChar:c];
        }
    }
    
    [self release];
    self = s;
    return self;
}

+ (NSInteger)formattedIntegerValueOfString:s
{
    NSInteger sum, pos, i, count = [s length];
    unichar digit;
    
    sum = 0;
    pos = 1;
    for( i = count-1; i>= 0; i-- ) {
        digit = [s characterAtIndex:i];
        if( digit >= '0' && digit <='9' ) {
            sum = sum + (digit-'0')*pos;
            pos *= 10;
        } else if( digit == '.' ) {
            //truncate fractions
            sum = 0;
            pos = 1;
        }
    }
    return sum;
}

//returns an integer value from a string with commas
- (NSInteger)formattedIntegerValue
{
    return [NSMutableString formattedIntegerValueOfString:self];
}

- (void)padToLength:(NSInteger)len {
    while( [self length] < len ) {
        [self appendString:@" "];
    }
}


+ (NSMutableString*)stringToNil:(NSString*)s {
    
    NSMutableString *n;
    if( nil == s ) {
        n = [NSMutableString stringWithString:@"nil"];
    } else {
        n = [NSMutableString stringWithString:s];
    }
    return n;
}

+ (NSMutableString*)nilToString:(NSString*)s {
    
    NSMutableString *n = nil;
    
    if( [s compare:@"nil" options:NSCaseInsensitiveSearch] != NSOrderedSame ) {
        n = [NSMutableString stringWithString:s];
    }
    return n;
}

@end
