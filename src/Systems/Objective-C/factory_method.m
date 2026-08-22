#import <Foundation/Foundation.h>

@protocol FactoryMethodDatabase <NSObject>
- (void)connect;
- (void)query;
@end

@interface FactoryMethodPostgres : NSObject <FactoryMethodDatabase>
@end

@implementation FactoryMethodPostgres
- (void)connect { NSLog(@"PostgreSQL connect"); }
- (void)query { NSLog(@"PostgreSQL query"); }
@end

@interface FactoryMethodMySql : NSObject <FactoryMethodDatabase>
@end

@implementation FactoryMethodMySql
- (void)connect { NSLog(@"MySQL connect"); }
- (void)query { NSLog(@"MySQL query"); }
@end

@interface FactoryMethodCreator : NSObject
- (id<FactoryMethodDatabase>)createDatabase;
- (void)useDatabase;
@end

@implementation FactoryMethodCreator
- (id<FactoryMethodDatabase>)createDatabase {
    [NSException raise:NSInternalInconsistencyException format:@"Subclass must override createDatabase"];
    return nil;
}

- (void)useDatabase {
    id<FactoryMethodDatabase> database = [self createDatabase];
    [database connect];
    [database query];
}
@end

@interface FactoryMethodPostgresCreator : FactoryMethodCreator
@end

@implementation FactoryMethodPostgresCreator
- (id<FactoryMethodDatabase>)createDatabase { return [[FactoryMethodPostgres alloc] init]; }
@end

@interface FactoryMethodMySqlCreator : FactoryMethodCreator
@end

@implementation FactoryMethodMySqlCreator
- (id<FactoryMethodDatabase>)createDatabase { return [[FactoryMethodMySql alloc] init]; }
@end

int main(void) {
    @autoreleasepool {
        [[[FactoryMethodPostgresCreator alloc] init] useDatabase];
        [[[FactoryMethodMySqlCreator alloc] init] useDatabase];
    }
    return 0;
}
