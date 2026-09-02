#import "memento.h"
#import <stdio.h>

@interface MementoSnapshot : NSObject
@property(nonatomic, copy, readonly) NSString *state;
- (instancetype)initWithState:(NSString *)state;
@end

@implementation MementoSnapshot
- (instancetype)initWithState:(NSString *)state {
    if ((self = [super init])) {
        _state = [state copy];
    }
    return self;
}
@end

@interface MementoDocument : NSObject
@property(nonatomic, copy) NSString *state;
- (instancetype)initWithState:(NSString *)state;
- (MementoSnapshot *)save;
- (void)restore:(MementoSnapshot *)snapshot;
@end

@implementation MementoDocument
- (instancetype)initWithState:(NSString *)state {
    if ((self = [super init])) {
        _state = [state copy];
    }
    return self;
}

- (MementoSnapshot *)save {
    return [[MementoSnapshot alloc] initWithState:self.state];
}

- (void)restore:(MementoSnapshot *)snapshot {
    self.state = snapshot.state;
}
@end

BOOL verifyMementoCanonical(void) {
    MementoDocument *document = [[MementoDocument alloc] initWithState:@"draft"];
    MementoSnapshot *snapshot = [document save];

    document.state = @"published";
    BOOL changed = [document.state isEqualToString:@"published"];
    BOOL snapshotPreserved = [snapshot.state isEqualToString:@"draft"];

    [document restore:snapshot];
    BOOL restored = [document.state isEqualToString:@"draft"];

    document.state = @"archived";
    BOOL independent = [snapshot.state isEqualToString:@"draft"];

    return changed && snapshotPreserved && restored && independent;
}

#ifdef MEMENTO_STANDALONE
int main(void) {
    @autoreleasepool {
        if (!verifyMementoCanonical()) {
            return 1;
        }
        puts("Objective-C Memento: passed");
        return 0;
    }
}
#endif
