#import <Foundation/Foundation.h>

@protocol GKObserver <NSObject>
- (void)updateWithState:(NSString *)state;
@end

@interface GKRecordingObserver : NSObject <GKObserver> {
    NSString *_name;
    NSMutableArray *_events;
}
- (instancetype)initWithName:(NSString *)name;
- (NSArray *)events;
@end

@implementation GKRecordingObserver
- (instancetype)initWithName:(NSString *)name {
    if ((self = [super init])) {
        _name = [name copy];
        _events = [NSMutableArray array];
    }
    return self;
}

- (void)updateWithState:(NSString *)state {
    [_events addObject:[NSString stringWithFormat:@"%@:%@", _name, state]];
}

- (NSArray *)events {
    return _events;
}
@end

@interface GKDocumentSubject : NSObject {
    NSMutableArray *_observers;
    NSString *_state;
}
- (void)subscribe:(id<GKObserver>)observer;
- (void)unsubscribe:(id<GKObserver>)observer;
- (void)publishState:(NSString *)state;
@end

@implementation GKDocumentSubject
- (instancetype)init {
    if ((self = [super init])) {
        _observers = [NSMutableArray array];
        _state = @"draft";
    }
    return self;
}

- (void)subscribe:(id<GKObserver>)observer {
    if (![_observers containsObject:observer]) {
        [_observers addObject:observer];
    }
}

- (void)unsubscribe:(id<GKObserver>)observer {
    [_observers removeObject:observer];
}

- (void)publishState:(NSString *)state {
    _state = [state copy];
    for (id<GKObserver> observer in [NSArray arrayWithArray:_observers]) {
        [observer updateWithState:_state];
    }
}
@end

BOOL observerExamplePasses(void) {
    GKDocumentSubject *document = [GKDocumentSubject new];
    GKRecordingObserver *audit = [[GKRecordingObserver alloc] initWithName:@"audit"];
    GKRecordingObserver *dashboard = [[GKRecordingObserver alloc] initWithName:@"dashboard"];

    [document subscribe:audit];
    [document subscribe:dashboard];
    [document subscribe:dashboard];
    [document publishState:@"published"];

    BOOL firstNotification = [[audit events] isEqualToArray:@[@"audit:published"]]
        && [[dashboard events] isEqualToArray:@[@"dashboard:published"]];

    [document unsubscribe:dashboard];
    [document publishState:@"archived"];

    BOOL unsubscribeWorked = [[audit events] isEqualToArray:@[@"audit:published", @"audit:archived"]]
        && [[dashboard events] isEqualToArray:@[@"dashboard:published"]];

    return firstNotification && unsubscribeWorked;
}

#ifndef GENKIDAMA_OBSERVER_NO_MAIN
int main(void) {
    @autoreleasepool {
        if (!observerExamplePasses()) {
            return 1;
        }
        NSLog(@"Objective-C Observer: passed");
    }
    return 0;
}
#endif
