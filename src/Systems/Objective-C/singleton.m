#import <Foundation/Foundation.h>

@interface Registry : NSObject
@property(nonatomic) NSInteger count;
+ (instancetype)sharedRegistry;
@end

@implementation Registry
+ (instancetype)sharedRegistry {
    static Registry *shared = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        shared = [[Registry alloc] init];
    });
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
