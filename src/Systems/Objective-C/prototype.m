#import <Foundation/Foundation.h>

@interface ServiceProfile : NSObject <NSCopying>
@property(nonatomic, copy) NSString *name;
@property(nonatomic, strong) NSMutableArray<NSString *> *features;
- (instancetype)initWithName:(NSString *)name features:(NSArray<NSString *> *)features;
- (NSString *)describe;
@end

@implementation ServiceProfile
- (instancetype)initWithName:(NSString *)name features:(NSArray<NSString *> *)features {
    self = [super init];
    if (self) {
        _name = [name copy];
        _features = [features mutableCopy];
    }
    return self;
}

- (id)copyWithZone:(NSZone *)zone {
    ServiceProfile *copy = [[[self class] allocWithZone:zone] initWithName:self.name features:self.features];
    return copy;
}

- (NSString *)describe {
    return [NSString stringWithFormat:@"%@: %@", self.name, [self.features componentsJoinedByString:@","]];
}
@end

int main(void) {
    @autoreleasepool {
        ServiceProfile *original = [[ServiceProfile alloc] initWithName:@"orders" features:@[@"metrics"]];
        ServiceProfile *canary = [original copy];
        canary.name = @"orders-canary";
        [canary.features addObject:@"tracing"];

        NSLog(@"original=%@", [original describe]);
        NSLog(@"clone=%@", [canary describe]);
    }
    return 0;
}
