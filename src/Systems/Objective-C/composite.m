#import <Foundation/Foundation.h>

@protocol GKComponent <NSObject>
- (NSInteger)size;
@end

@interface GKFileLeaf : NSObject <GKComponent>
- (instancetype)initWithBytes:(NSInteger)bytes;
@end

@implementation GKFileLeaf {
    NSInteger _bytes;
}

- (instancetype)initWithBytes:(NSInteger)bytes {
    self = [super init];
    if (self) {
        _bytes = bytes;
    }
    return self;
}

- (NSInteger)size {
    return _bytes;
}
@end

@interface GKFolderComposite : NSObject <GKComponent>
- (instancetype)initWithChildren:(NSArray<id<GKComponent>> *)children;
@end

@implementation GKFolderComposite {
    NSArray<id<GKComponent>> *_children;
}

- (instancetype)initWithChildren:(NSArray<id<GKComponent>> *)children {
    self = [super init];
    if (self) {
        _children = [children copy];
    }
    return self;
}

- (NSInteger)size {
    NSInteger total = 0;
    for (id<GKComponent> child in _children) {
        total += [child size];
    }
    return total;
}
@end

int main(void) {
    @autoreleasepool {
        id<GKComponent> readme = [[GKFileLeaf alloc] initWithBytes:2];
        id<GKComponent> docs = [[GKFolderComposite alloc] initWithChildren:@[
            [[GKFileLeaf alloc] initWithBytes:3],
            [[GKFileLeaf alloc] initWithBytes:5]
        ]];
        id<GKComponent> root = [[GKFolderComposite alloc] initWithChildren:@[readme, docs]];

        printf("leaf=%ld\n", (long)[readme size]);
        printf("docs=%ld\n", (long)[docs size]);
        printf("root=%ld\n", (long)[root size]);
    }
    return 0;
}
