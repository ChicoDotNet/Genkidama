#import <Foundation/Foundation.h>

@protocol InterpreterExpression <NSObject>
- (NSInteger)interpret;
@end

@interface NumberExpression : NSObject <InterpreterExpression> {
    NSInteger _value;
}
- (instancetype)initWithValue:(NSInteger)value;
@end

@implementation NumberExpression

- (instancetype)initWithValue:(NSInteger)value {
    self = [super init];
    if (self) {
        _value = value;
    }
    return self;
}

- (NSInteger)interpret {
    return _value;
}
@end

@interface AddExpression : NSObject <InterpreterExpression> {
    id<InterpreterExpression> _left;
    id<InterpreterExpression> _right;
}
- (instancetype)initWithLeft:(id<InterpreterExpression>)left right:(id<InterpreterExpression>)right;
@end

@implementation AddExpression

- (instancetype)initWithLeft:(id<InterpreterExpression>)left right:(id<InterpreterExpression>)right {
    self = [super init];
    if (self) {
        _left = left;
        _right = right;
    }
    return self;
}

- (NSInteger)interpret {
    return [_left interpret] + [_right interpret];
}
@end

int main(void) {
    @autoreleasepool {
        id<InterpreterExpression> expression = [[AddExpression alloc]
            initWithLeft:[[AddExpression alloc]
                initWithLeft:[[NumberExpression alloc] initWithValue:2]
                right:[[NumberExpression alloc] initWithValue:3]]
            right:[[NumberExpression alloc] initWithValue:4]];
        NSInteger result = [expression interpret];
        if (result != 9) {
            NSLog(@"Interpreter expected 9, got %ld", (long)result);
            return 1;
        }
        NSLog(@"interpreter=9");
    }
    return 0;
}
