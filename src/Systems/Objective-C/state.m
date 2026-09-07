#import <Foundation/Foundation.h>

@class Turnstile;

@protocol GateState <NSObject>
- (NSString *)name;
- (NSString *)coin:(Turnstile *)turnstile;
- (NSString *)push:(Turnstile *)turnstile;
@end

@interface LockedState : NSObject <GateState>
@end

@interface UnlockedState : NSObject <GateState>
@end

@interface Turnstile : NSObject {
    id<GateState> _state;
}
- (instancetype)init;
- (NSString *)stateName;
- (NSString *)coin;
- (NSString *)push;
- (void)transitionTo:(id<GateState>)state;
@end

@implementation Turnstile
- (instancetype)init {
    if ((self = [super init])) {
        _state = [LockedState new];
    }
    return self;
}

- (NSString *)stateName { return [_state name]; }
- (NSString *)coin { return [_state coin:self]; }
- (NSString *)push { return [_state push:self]; }
- (void)transitionTo:(id<GateState>)state { _state = state; }
@end

@implementation LockedState
- (NSString *)name { return @"locked"; }
- (NSString *)coin:(Turnstile *)turnstile {
    [turnstile transitionTo:[UnlockedState new]];
    return @"unlocked";
}
- (NSString *)push:(Turnstile *)turnstile {
    (void)turnstile;
    return @"blocked";
}
@end

@implementation UnlockedState
- (NSString *)name { return @"unlocked"; }
- (NSString *)coin:(Turnstile *)turnstile {
    (void)turnstile;
    return @"coin-returned";
}
- (NSString *)push:(Turnstile *)turnstile {
    [turnstile transitionTo:[LockedState new]];
    return @"passed";
}
@end

static void require(BOOL condition, NSString *message) {
    if (!condition) {
        NSLog(@"State contract failed: %@", message);
        abort();
    }
}

int main(void) {
    @autoreleasepool {
        Turnstile *turnstile = [Turnstile new];

        require([[turnstile stateName] isEqualToString:@"locked"], @"initial state");
        require([[turnstile push] isEqualToString:@"blocked"], @"invalid push result");
        require([[turnstile stateName] isEqualToString:@"locked"], @"invalid push keeps state");
        require([[turnstile coin] isEqualToString:@"unlocked"], @"coin unlocks");
        require([[turnstile stateName] isEqualToString:@"unlocked"], @"unlocked state");
        require([[turnstile coin] isEqualToString:@"coin-returned"], @"duplicate coin result");
        require([[turnstile stateName] isEqualToString:@"unlocked"], @"duplicate coin keeps state");
        require([[turnstile push] isEqualToString:@"passed"], @"push passes");
        require([[turnstile stateName] isEqualToString:@"locked"], @"push locks again");

        puts("objective-c-state: passed");
    }
    return 0;
}
