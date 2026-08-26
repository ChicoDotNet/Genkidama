#import <Foundation/Foundation.h>

@interface TextStyle : NSObject
@property(nonatomic, copy, readonly) NSString *font;
@property(nonatomic, readonly) NSInteger size;
@property(nonatomic, copy, readonly) NSString *color;
- (instancetype)initWithFont:(NSString *)font size:(NSInteger)size color:(NSString *)color;
@end

@implementation TextStyle
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

@interface StyleFactory : NSObject
@property(nonatomic, strong, readonly) NSMutableDictionary<NSString *, TextStyle *> *pool;
- (TextStyle *)styleWithFont:(NSString *)font size:(NSInteger)size color:(NSString *)color;
@end

@implementation StyleFactory
- (instancetype)init {
    self = [super init];
    if (self) {
        _pool = [NSMutableDictionary dictionary];
    }
    return self;
}

- (TextStyle *)styleWithFont:(NSString *)font size:(NSInteger)size color:(NSString *)color {
    NSString *key = [NSString stringWithFormat:@"%@|%ld|%@", font, (long)size, color];
    TextStyle *style = self.pool[key];
    if (style == nil) {
        style = [[TextStyle alloc] initWithFont:font size:size color:color];
        self.pool[key] = style;
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
