#import <Foundation/Foundation.h>

@interface ServiceProfile : NSObject <NSCopying> {
    NSString *_name;
    NSMutableArray<NSString *> *_features;
}
@property(nonatomic, copy) NSString *name;
@property(nonatomic, strong) NSMutableArray<NSString *> *features;
- (instancetype)initWithName:(NSString *)name features:(NSArray<NSString *> *)features;
- (NSString *)describe;
@end

@implementation ServiceProfile
@synthesize name = _name;
@synthesize features = _features;

- (instancetype)initWithName:(NSString *)name features:(NSArray<NSString *> *)features {
    self = [super init];
    if (self) {
        self.name = name;
        self.features = [features mutableCopy];
    }
    return self;
}

- (id)copyWithZone:(NSZone *)zone {
    return [[[self class] allocWithZone:zone] initWithName:self.name features:self.features];
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
