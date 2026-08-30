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
    NSNumber *value = [_values objectAtIndex:_index];
    _index += 1;
    return value;
}
@end

static BOOL iteratorExamplePasses(void) {
    NumberIterator *iterator = [[NumberIterator alloc] initWithValues:@[@10, @20, @30]];
    NSMutableArray<NSNumber *> *visited = [NSMutableArray array];
    while ([iterator hasNext]) {
        [visited addObject:[iterator next]];
    }
    return [visited isEqualToArray:@[@10, @20, @30]] && ![iterator hasNext];
}

#ifndef GENKIDAMA_ITERATOR_EMBEDDED
int main(void) {
    @autoreleasepool {
        if (!iteratorExamplePasses()) {
            return 1;
        }
        printf("iterator=10,20,30\n");
    }
    return 0;
}
#endif
