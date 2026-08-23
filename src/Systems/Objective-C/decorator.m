#import <Foundation/Foundation.h>

@protocol Component <NSObject>
- (NSString *)render;
@end

@interface PlainMessage : NSObject <Component>
@end
@implementation PlainMessage
- (NSString *)render { return @"alert"; }
@end

@interface ComponentDecorator : NSObject <Component>
@property(nonatomic, strong) id<Component> inner;
- (instancetype)initWithInner:(id<Component>)inner;
@end
@implementation ComponentDecorator
- (instancetype)initWithInner:(id<Component>)inner {
    if ((self = [super init])) { _inner = inner; }
    return self;
}
- (NSString *)render { return [self.inner render]; }
@end

@interface AuditDecorator : ComponentDecorator @end
@implementation AuditDecorator
- (NSString *)render { return [NSString stringWithFormat:@"audit(%@)", [self.inner render]]; }
@end

@interface EncryptDecorator : ComponentDecorator @end
@implementation EncryptDecorator
- (NSString *)render { return [NSString stringWithFormat:@"enc(%@)", [self.inner render]]; }
@end

int main(void) {
    @autoreleasepool {
        id<Component> base = [PlainMessage new];
        id<Component> audited = [[AuditDecorator alloc] initWithInner:base];
        id<Component> encrypted = [[EncryptDecorator alloc] initWithInner:base];
        id<Component> stacked = [[AuditDecorator alloc] initWithInner:[[EncryptDecorator alloc] initWithInner:base]];
        printf("base=%s\n", [[base render] UTF8String]);
        printf("audit=%s\n", [[audited render] UTF8String]);
        printf("encrypted=%s\n", [[encrypted render] UTF8String]);
        printf("stacked=%s\n", [[stacked render] UTF8String]);
    }
    return 0;
}
