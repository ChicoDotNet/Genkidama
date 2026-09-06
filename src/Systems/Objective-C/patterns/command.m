#import <Foundation/Foundation.h>

@protocol Command <NSObject>
- (NSInteger)execute:(NSInteger)balance;
@end

@interface DepositCommand : NSObject <Command> {
    NSInteger _amount;
}
- (instancetype)initWithAmount:(NSInteger)amount;
@end
@implementation DepositCommand
- (instancetype)initWithAmount:(NSInteger)amount { if ((self = [super init])) { _amount = amount; } return self; }
- (NSInteger)execute:(NSInteger)balance { return balance + _amount; }
@end

@interface WithdrawCommand : NSObject <Command> {
    NSInteger _amount;
}
- (instancetype)initWithAmount:(NSInteger)amount;
@end
@implementation WithdrawCommand
- (instancetype)initWithAmount:(NSInteger)amount { if ((self = [super init])) { _amount = amount; } return self; }
- (NSInteger)execute:(NSInteger)balance { return balance - _amount; }
@end

int main(void) {
    @autoreleasepool {
        NSArray<id<Command>> *queue = @[
            [[DepositCommand alloc] initWithAmount:50],
            [[WithdrawCommand alloc] initWithAmount:20]
        ];
        NSInteger balance = 100;
        for (id<Command> command in queue) {
            balance = [command execute:balance];
        }
        NSCAssert(balance == 130, @"Command contract failed");
        NSCAssert(queue.count == 2, @"Command queue contract failed");
        printf("balance=%ld;commands=%lu\n", (long)balance, (unsigned long)queue.count);
    }
    return 0;
}
