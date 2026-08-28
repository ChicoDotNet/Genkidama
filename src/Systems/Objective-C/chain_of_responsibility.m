#import <Foundation/Foundation.h>

@protocol RefundHandler <NSObject>
- (NSString *)name;
- (BOOL)canHandleAmount:(NSInteger)amount;
- (NSString *)handleAmount:(NSInteger)amount visited:(NSMutableArray<NSString *> *)visited;
@end

@interface Handler : NSObject <RefundHandler>
@property(nonatomic, copy) NSString *handlerName;
@property(nonatomic) NSInteger limit;
@property(nonatomic, strong, nullable) Handler *next;
- (instancetype)initWithName:(NSString *)name limit:(NSInteger)limit next:(nullable Handler *)next;
@end

@implementation Handler
- (instancetype)initWithName:(NSString *)name limit:(NSInteger)limit next:(Handler *)next {
    self = [super init];
    if (self) {
        _handlerName = [name copy];
        _limit = limit;
        _next = next;
    }
    return self;
}

- (NSString *)name { return self.handlerName; }
- (BOOL)canHandleAmount:(NSInteger)amount { return amount <= self.limit; }

- (NSString *)handleAmount:(NSInteger)amount visited:(NSMutableArray<NSString *> *)visited {
    [visited addObject:self.handlerName];
    if ([self canHandleAmount:amount]) {
        return [NSString stringWithFormat:@"handled=%@;result=refund(%ld)", self.handlerName, (long)amount];
    }
    if (self.next == nil) {
        return @"handled=none;result=rejected";
    }
    return [self.next handleAmount:amount visited:visited];
}
@end

int main(void) {
    @autoreleasepool {
        Handler *escalation = [[Handler alloc] initWithName:@"escalation" limit:NSIntegerMax next:nil];
        Handler *billing = [[Handler alloc] initWithName:@"billing" limit:500 next:escalation];
        Handler *faq = [[Handler alloc] initWithName:@"faq" limit:50 next:billing];
        NSMutableArray<NSString *> *visited = [NSMutableArray array];
        NSString *result = [faq handleAmount:250 visited:visited];
        printf("visited=%s;%s\n", [[visited componentsJoinedByString:@">"] UTF8String], [result UTF8String]);
    }
    return 0;
}
