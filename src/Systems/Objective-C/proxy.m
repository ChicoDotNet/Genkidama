#import <Foundation/Foundation.h>

@protocol DocumentStore <NSObject>
- (NSString *)documentWithId:(NSInteger)documentId;
@end

@interface RemoteDocumentStore : NSObject <DocumentStore>
@property(nonatomic) NSInteger fetches;
@end

@implementation RemoteDocumentStore
- (NSString *)documentWithId:(NSInteger)documentId {
    self.fetches += 1;
    return [NSString stringWithFormat:@"doc(%ld)", (long)documentId];
}
@end

@interface DocumentStoreProxy : NSObject <DocumentStore>
@property(nonatomic, strong) RemoteDocumentStore *backend;
@property(nonatomic, strong) NSMutableDictionary<NSNumber *, NSString *> *cache;
@property(nonatomic) NSInteger backendCreations;
@end

@implementation DocumentStoreProxy
- (instancetype)init {
    self = [super init];
    if (self) {
        _cache = [NSMutableDictionary dictionary];
    }
    return self;
}

- (NSString *)documentWithId:(NSInteger)documentId {
    NSNumber *key = @(documentId);
    NSString *cached = self.cache[key];
    if (cached != nil) {
        return cached;
    }
    if (self.backend == nil) {
        self.backend = [[RemoteDocumentStore alloc] init];
        self.backendCreations += 1;
    }
    NSString *value = [self.backend documentWithId:documentId];
    self.cache[key] = value;
    return value;
}
@end

int main(void) {
    @autoreleasepool {
        DocumentStoreProxy *store = [[DocumentStoreProxy alloc] init];
        NSString *first = [store documentWithId:42];
        NSString *second = [store documentWithId:42];
        printf("backend=%ld;fetches=%ld;first=%s;second=%s\n",
               (long)store.backendCreations,
               (long)store.backend.fetches,
               first.UTF8String,
               second.UTF8String);
    }
    return 0;
}
