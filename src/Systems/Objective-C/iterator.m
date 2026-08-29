#import <Foundation/Foundation.h>

@interface NumberIterator : NSObject {
    NSArray<NSNumber *> *_values;
    NSUInteger _index;
}
- (instancetype)initWithValues:(NSArray<NSNumber *> *)values;
- (BOOL)hasNext;
- (NSNumber *)next;
@end

@implementation NumberIterator
- (instancetype)initWithValues:(NSArray<NSNumber *> *)values {
    self = [super init];
    if (self) {
        _values = [values copy];
        _index = 0;
    }
    return self;
}

- (BOOL)hasNext {
    return _index < [_values count];
}

- (NSNumber *)next {
    return _values[_index++];
}
@end

int main(void) {
    @autoreleasepool {
        NumberIterator *iterator = [[NumberIterator alloc] initWithValues:@[@10, @20, @30]];
        NSMutableArray<NSNumber *> *visited = [NSMutableArray array];
        while ([iterator hasNext]) {
            [visited addObject:[iterator next]];
        }
        if (![visited isEqualToArray:@[@10, @20, @30]] || [iterator hasNext]) {
            return 1;
        }
        printf("iterator=10,20,30\n");
    }
    return 0;
}
