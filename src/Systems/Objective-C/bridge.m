#import <Foundation/Foundation.h>

@protocol BridgeDevice <NSObject>
- (NSString *)powerOn;
- (NSString *)mute;
@end

@interface TvDevice : NSObject <BridgeDevice>
@end
@implementation TvDevice
- (NSString *)powerOn { return @"TV:on"; }
- (NSString *)mute { return @"TV:muted"; }
@end

@interface RadioDevice : NSObject <BridgeDevice>
@end
@implementation RadioDevice
- (NSString *)powerOn { return @"Radio:on"; }
- (NSString *)mute { return @"Radio:muted"; }
@end

@interface BasicRemote : NSObject {
    id<BridgeDevice> _device;
}
@property(nonatomic, strong) id<BridgeDevice> device;
- (instancetype)initWithDevice:(id<BridgeDevice>)device;
- (NSString *)activate;
@end
@implementation BasicRemote
@synthesize device = _device;

- (instancetype)initWithDevice:(id<BridgeDevice>)device {
    self = [super init];
    if (self) { self.device = device; }
    return self;
}
- (NSString *)activate { return [self.device powerOn]; }
@end

@interface MuteRemote : NSObject {
    id<BridgeDevice> _device;
}
@property(nonatomic, strong) id<BridgeDevice> device;
- (instancetype)initWithDevice:(id<BridgeDevice>)device;
- (NSString *)activate;
@end
@implementation MuteRemote
@synthesize device = _device;

- (instancetype)initWithDevice:(id<BridgeDevice>)device {
    self = [super init];
    if (self) { self.device = device; }
    return self;
}
- (NSString *)activate { return [self.device mute]; }
@end

int main(void) {
    @autoreleasepool {
        TvDevice *tv = [TvDevice new];
        RadioDevice *radio = [RadioDevice new];
        printf("basic-tv=%s\n", [[[BasicRemote alloc] initWithDevice:tv].activate UTF8String]);
        printf("basic-radio=%s\n", [[[BasicRemote alloc] initWithDevice:radio].activate UTF8String]);
        printf("mute-tv=%s\n", [[[MuteRemote alloc] initWithDevice:tv].activate UTF8String]);
        printf("mute-radio=%s\n", [[[MuteRemote alloc] initWithDevice:radio].activate UTF8String]);
    }
    return 0;
}
