#import <Foundation/Foundation.h>

@interface Registry : NSObject {
    NSInteger _count;
}
@property(nonatomic) NSInteger count;
+ (instancetype)sharedRegistry;
@end

@implementation Registry
@synthesize count = _count;

+ (instancetype)sharedRegistry {
    static Registry *shared = nil;
    @synchronized(self) {
        if (shared == nil) {
            shared = [[Registry alloc] init];
        }
    }
    return shared;
}
@end

int main(void) {
    @autoreleasepool {
        Registry *first = [Registry sharedRegistry];
        Registry *second = [Registry sharedRegistry];
        first.count += 1;
        NSLog(@"same=%@", first == second ? @"true" : @"false");
        NSLog(@"count=%ld", (long)second.count);
    }
    return 0;
}
