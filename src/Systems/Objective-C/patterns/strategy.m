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

BOOL verifyStrategy(void) {
    id<PricingStrategy> regular = [RegularPricing new];
    id<PricingStrategy> vip = [VipPricing new];
    return ApplyPrice(100, regular) == 100 && ApplyPrice(100, vip) == 80;
}

#ifndef GENKIDAMA_STRATEGY_EMBEDDED
int main(void) {
    @autoreleasepool {
        NSCAssert(verifyStrategy(), @"Objective-C Strategy contract failed");
        puts("Objective-C Strategy: passed");
    }
    return 0;
}
#endif
