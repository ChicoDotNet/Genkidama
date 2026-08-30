#import <Foundation/Foundation.h>

@interface AuthService : NSObject
- (NSString *)authenticate:(NSString *)user;
@end
@implementation AuthService
- (NSString *)authenticate:(NSString *)user {
    return [NSString stringWithFormat:@"auth(%@)", user];
}
@end

@interface InventoryService : NSObject
- (NSString *)reserve:(NSString *)sku;
@end
@implementation InventoryService
- (NSString *)reserve:(NSString *)sku {
    return [NSString stringWithFormat:@"reserve(%@)", sku];
}
@end

@interface BillingService : NSObject
- (NSString *)charge:(NSInteger)amount;
@end
@implementation BillingService
- (NSString *)charge:(NSInteger)amount {
    return [NSString stringWithFormat:@"charge(%ld)", (long)amount];
}
@end

@interface CheckoutFacade : NSObject {
    AuthService *_auth;
    InventoryService *_inventory;
    BillingService *_billing;
}
@property(nonatomic, strong) AuthService *auth;
@property(nonatomic, strong) InventoryService *inventory;
@property(nonatomic, strong) BillingService *billing;
- (instancetype)initWithAuth:(AuthService *)auth inventory:(InventoryService *)inventory billing:(BillingService *)billing;
- (NSString *)checkoutUser:(NSString *)user sku:(NSString *)sku amount:(NSInteger)amount;
@end
@implementation CheckoutFacade
@synthesize auth = _auth;
@synthesize inventory = _inventory;
@synthesize billing = _billing;

- (instancetype)initWithAuth:(AuthService *)auth inventory:(InventoryService *)inventory billing:(BillingService *)billing {
    self = [super init];
    if (self) {
        self.auth = auth;
        self.inventory = inventory;
        self.billing = billing;
    }
    return self;
}
- (NSString *)checkoutUser:(NSString *)user sku:(NSString *)sku amount:(NSInteger)amount {
    return [NSString stringWithFormat:@"checkout=%@>%@>%@",
            [self.auth authenticate:user],
            [self.inventory reserve:sku],
            [self.billing charge:amount]];
}
@end

int main(void) {
    @autoreleasepool {
        CheckoutFacade *facade = [[CheckoutFacade alloc]
            initWithAuth:[AuthService new]
            inventory:[InventoryService new]
            billing:[BillingService new]];
        printf("%s\n", [[facade checkoutUser:@"alice" sku:@"SKU-42" amount:499] UTF8String]);
    }
    return 0;
}
