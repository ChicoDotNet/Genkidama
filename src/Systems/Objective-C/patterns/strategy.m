#import <Foundation/Foundation.h>

@protocol PricingStrategy <NSObject>
- (NSInteger)price:(NSInteger)value;
@end

@interface RegularPricing : NSObject <PricingStrategy>
@end

@implementation RegularPricing
- (NSInteger)price:(NSInteger)value { return value; }
@end

@interface VipPricing : NSObject <PricingStrategy>
@end

@implementation VipPricing
- (NSInteger)price:(NSInteger)value { return value * 80 / 100; }
@end

static NSInteger ApplyPrice(NSInteger value, id<PricingStrategy> strategy) {
    return [strategy price:value];
}

int main(void) {
    @autoreleasepool {
        id<PricingStrategy> regular = [RegularPricing new];
        id<PricingStrategy> vip = [VipPricing new];
        if (ApplyPrice(100, regular) != 100 || ApplyPrice(100, vip) != 80) {
            NSLog(@"Strategy contract failed");
            return 1;
        }
        NSLog(@"regular=100;vip=80");
    }
    return 0;
}
