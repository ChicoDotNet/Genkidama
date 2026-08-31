#import <Foundation/Foundation.h>

@protocol MediatorColleague <NSObject>
- (NSString *)receiveFrom:(NSString *)sender message:(NSString *)message;
@end

@interface CheckoutMediator : NSObject {
    NSMutableDictionary *_colleagues;
}
- (void)registerColleague:(id<MediatorColleague>)colleague named:(NSString *)name;
- (NSString *)sendFrom:(NSString *)sender to:(NSString *)recipient message:(NSString *)message;
@end

@implementation CheckoutMediator
- (instancetype)init {
    if ((self = [super init])) {
        _colleagues = [NSMutableDictionary dictionary];
    }
    return self;
}

- (void)registerColleague:(id<MediatorColleague>)colleague named:(NSString *)name {
    [_colleagues setObject:colleague forKey:name];
}

- (NSString *)sendFrom:(NSString *)sender to:(NSString *)recipient message:(NSString *)message {
    id<MediatorColleague> colleague = [_colleagues objectForKey:recipient];
    if (colleague == nil) {
        [NSException raise:@"UnknownColleague"
                    format:@"unknown colleague: %@", recipient];
    }
    return [colleague receiveFrom:sender message:message];
}
@end

@interface PaymentColleague : NSObject <MediatorColleague>
@end

@implementation PaymentColleague
- (NSString *)receiveFrom:(NSString *)sender message:(NSString *)message {
    return [NSString stringWithFormat:@"payment<-%@:%@", sender, message];
}
@end

@interface InventoryColleague : NSObject <MediatorColleague>
@end

@implementation InventoryColleague
- (NSString *)receiveFrom:(NSString *)sender message:(NSString *)message {
    return [NSString stringWithFormat:@"inventory<-%@:%@", sender, message];
}
@end

static BOOL verifyMediator(void) {
    CheckoutMediator *mediator = [CheckoutMediator new];
    [mediator registerColleague:[PaymentColleague new] named:@"payment"];
    [mediator registerColleague:[InventoryColleague new] named:@"inventory"];

    NSString *paid = [mediator sendFrom:@"payment" to:@"inventory" message:@"paid"];
    NSString *reserved = [mediator sendFrom:@"inventory" to:@"payment" message:@"reserved"];

    BOOL rejectedUnknown = NO;
    @try {
        (void)[mediator sendFrom:@"payment" to:@"shipping" message:@"dispatch"];
    }
    @catch (NSException *exception) {
        rejectedUnknown = [[exception name] isEqualToString:@"UnknownColleague"] &&
            [[exception reason] isEqualToString:@"unknown colleague: shipping"];
    }

    return [paid isEqualToString:@"inventory<-payment:paid"] &&
        [reserved isEqualToString:@"payment<-inventory:reserved"] &&
        rejectedUnknown;
}

int main(void) {
    @autoreleasepool {
        NSCAssert(verifyMediator(), @"Objective-C Mediator contract failed");
        puts("Objective-C Mediator: passed");
    }
    return 0;
}
