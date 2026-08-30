#import <Foundation/Foundation.h>

@interface TextStyle : NSObject {
    NSString *_font;
    NSInteger _size;
    NSString *_color;
}
@property(nonatomic, copy, readonly) NSString *font;
@property(nonatomic, readonly) NSInteger size;
@property(nonatomic, copy, readonly) NSString *color;
- (instancetype)initWithFont:(NSString *)font size:(NSInteger)size color:(NSString *)color;
@end

@implementation TextStyle
@synthesize font = _font;
@synthesize size = _size;
@synthesize color = _color;

- (instancetype)initWithFont:(NSString *)font size:(NSInteger)size color:(NSString *)color {
    self = [super init];
    if (self) {
        _font = [font copy];
        _size = size;
        _color = [color copy];
    }
    return self;
}
@end

@interface StyleFactory : NSObject {
    NSMutableDictionary<NSString *, TextStyle *> *_pool;
}
@property(nonatomic, strong, readonly) NSMutableDictionary<NSString *, TextStyle *> *pool;
- (TextStyle *)styleWithFont:(NSString *)font size:(NSInteger)size color:(NSString *)color;
@end

@implementation StyleFactory
@synthesize pool = _pool;

- (instancetype)init {
    self = [super init];
    if (self) {
        _pool = [NSMutableDictionary dictionary];
    }
    return self;
}

- (TextStyle *)styleWithFont:(NSString *)font size:(NSInteger)size color:(NSString *)color {
    NSString *key = [NSString stringWithFormat:@"%@|%ld|%@", font, (long)size, color];
    TextStyle *style = [self.pool objectForKey:key];
    if (style == nil) {
        style = [[TextStyle alloc] initWithFont:font size:size color:color];
        [self.pool setObject:style forKey:key];
    }
    return style;
}
@end

int main(void) {
    @autoreleasepool {
        StyleFactory *factory = [[StyleFactory alloc] init];
        TextStyle *red1 = [factory styleWithFont:@"Inter" size:12 color:@"red"];
        TextStyle *red2 = [factory styleWithFont:@"Inter" size:12 color:@"red"];
        TextStyle *blue = [factory styleWithFont:@"Inter" size:12 color:@"blue"];
        NSCAssert([blue.color isEqualToString:@"blue"], @"blue style missing");
        printf("styles=%lu;shared=%s;text=ABC\n", (unsigned long)factory.pool.count,
               red1 == red2 ? "true" : "false");
    }
    return 0;
}
