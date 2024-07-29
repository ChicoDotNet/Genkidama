#import <Foundation/Foundation.h>

// Abstract Product
@protocol Database <NSObject>
- (void)connect;
- (void)query;
@end

// Concrete Product
@interface Postgres : NSObject <Database>
@end

@implementation Postgres
- (void)connect {
    NSLog(@"Connecting to PostgreSQL");
}
- (void)query {
    NSLog(@"Querying PostgreSQL");
}
@end

@interface MySQL : NSObject <Database>
@end

@implementation MySQL
- (void)connect {
    NSLog(@"Connecting to MySQL");
}
- (void)query {
    NSLog(@"Querying MySQL");
}
@end

// Abstract Factory
@protocol DatabaseFactory <NSObject>
- (id<Database>)createDatabase;
@end

// Concrete Factory
@interface PostgresFactory : NSObject <DatabaseFactory>
@end

@implementation PostgresFactory
- (id<Database>)createDatabase {
    return [Postgres new];
}
@end

@interface MySQLFactory : NSObject <DatabaseFactory>
@end

@implementation MySQLFactory
- (id<Database>)createDatabase {
    return [MySQL new];
}
@end

void useDatabase(id<DatabaseFactory> factory) {
    id<Database> db = [factory createDatabase];
    [db connect];
    [db query];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        useDatabase([PostgresFactory new]);
        useDatabase([MySQLFactory new]);
    }
    return 0;
}
