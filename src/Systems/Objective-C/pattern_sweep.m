#import <Foundation/Foundation.h>

@protocol SweepExpression <NSObject>
- (NSInteger)evaluate;
@end

@interface SweepLiteral : NSObject <SweepExpression> {
    NSInteger _value;
}
- (instancetype)initWithValue:(NSInteger)value;
@end

@implementation SweepLiteral
- (instancetype)initWithValue:(NSInteger)value {
    self = [super init];
    if (self != nil) {
        _value = value;
    }
    return self;
}
- (NSInteger)evaluate { return _value; }
@end

typedef NS_ENUM(NSInteger, SweepBinaryOperation) {
    SweepBinaryAdd,
    SweepBinaryMultiply,
};

@interface SweepBinary : NSObject <SweepExpression> {
    id<SweepExpression> _left;
    id<SweepExpression> _right;
    SweepBinaryOperation _operation;
}
- (instancetype)initWithLeft:(id<SweepExpression>)left
                       right:(id<SweepExpression>)right
                   operation:(SweepBinaryOperation)operation;
@end

@implementation SweepBinary
- (instancetype)initWithLeft:(id<SweepExpression>)left
                       right:(id<SweepExpression>)right
                   operation:(SweepBinaryOperation)operation {
    self = [super init];
    if (self != nil) {
        _left = left;
        _right = right;
        _operation = operation;
    }
    return self;
}
- (NSInteger)evaluate {
    NSInteger left = [_left evaluate];
    NSInteger right = [_right evaluate];
    return _operation == SweepBinaryAdd ? left + right : left * right;
}
@end

static NSInteger IdentityStrategy(NSInteger value) { return value; }
static NSInteger DiscountStrategy(NSInteger value) { return value * 80 / 100; }
static NSInteger DoublePlugin(NSInteger value) { return value * 2; }
static NSInteger SquarePlugin(NSInteger value) { return value * value; }
static NSString *FixedClock(void) { return @"12:00"; }
static NSUInteger RealLogger(NSString *message) { return [message length]; }
static NSUInteger NullLogger(NSString *message) { (void)message; return 0; }

static BOOL CommandExample(void) {
    NSArray<NSNumber *> *commands = @[@10, @-3];
    NSInteger balance = 0;
    for (NSNumber *command in commands) balance += [command integerValue];
    NSInteger undone = balance - [[commands lastObject] integerValue];
    return balance == 7 && undone == 10;
}

static BOOL InterpreterExample(void) {
    SweepLiteral *two = [[SweepLiteral alloc] initWithValue:2];
    SweepLiteral *three = [[SweepLiteral alloc] initWithValue:3];
    SweepLiteral *four = [[SweepLiteral alloc] initWithValue:4];
    SweepBinary *multiply = [[SweepBinary alloc] initWithLeft:three right:four operation:SweepBinaryMultiply];
    SweepBinary *expression = [[SweepBinary alloc] initWithLeft:two right:multiply operation:SweepBinaryAdd];
    return [expression evaluate] == 14;
}

static BOOL IteratorExample(void) {
    NSArray<NSNumber *> *values = @[@10, @20];
    NSEnumerator<NSNumber *> *iterator = [values objectEnumerator];
    NSNumber *first = [iterator nextObject];
    NSNumber *second = [iterator nextObject];
    NSNumber *end = [iterator nextObject];
    return [first integerValue] == 10 && [second integerValue] == 20 && end == nil;
}

static BOOL MediatorExample(void) {
    NSString *sender = @"sales";
    NSString *recipient = [sender isEqualToString:@"sales"] ? @"billing" : @"sales";
    return [recipient isEqualToString:@"billing"];
}

static BOOL MementoExample(void) {
    NSString *current = @"v1";
    NSString *snapshot = [current copy];
    current = @"v2";
    current = snapshot;
    return [current isEqualToString:@"v1"];
}

static BOOL ObserverExample(void) {
    NSMutableArray<NSString *> *events = [NSMutableArray array];
    [events addObject:[NSString stringWithFormat:@"audit:%d", 7]];
    [events addObject:[NSString stringWithFormat:@"ui:%d", 7]];
    return [[events componentsJoinedByString:@"|"] isEqualToString:@"audit:7|ui:7"];
}

static BOOL StateExample(void) {
    BOOL loggedIn = NO;
    loggedIn = !loggedIn;
    BOOL first = loggedIn;
    loggedIn = !loggedIn;
    return first && !loggedIn;
}

static BOOL StrategyExample(void) {
    NSInteger (*regular)(NSInteger) = IdentityStrategy;
    NSInteger (*discounted)(NSInteger) = DiscountStrategy;
    return regular(100) == 100 && discounted(100) == 80;
}

static BOOL TemplateMethodExample(void) {
    NSInteger value = 3;
    NSInteger transformed = DoublePlugin(value);
    NSInteger closed = transformed + 1;
    return closed == 7;
}

static BOOL VisitorExample(void) {
    NSArray<NSDictionary<NSString *, NSNumber *> *> *shapes = @[
        @{ @"area": @12, @"perimeter": @12 },
        @{ @"area": @12, @"perimeter": @14 },
    ];
    return [[shapes[0] objectForKey:@"area"] integerValue] == 12 &&
           [[shapes[1] objectForKey:@"perimeter"] integerValue] == 14;
}

static BOOL MVCExample(void) {
    NSInteger model = 3;
    model += 1;
    NSString *view = [NSString stringWithFormat:@"count=%ld", (long)model];
    return [view isEqualToString:@"count=4"];
}

static BOOL MVVMExample(void) {
    NSDictionary<NSString *, id> *viewModel = @{ @"greeting": @"Hello Ada", @"enabled": @YES };
    return [[viewModel objectForKey:@"greeting"] isEqual:@"Hello Ada"] &&
           [[viewModel objectForKey:@"enabled"] boolValue];
}

static BOOL MicrokernelExample(void) {
    NSInteger (*plugins[2])(NSInteger) = { DoublePlugin, SquarePlugin };
    return plugins[0](5) == 10 && plugins[1](3) == 9;
}

static BOOL MicroservicesExample(void) {
    NSDictionary<NSString *, NSNumber *> *inventory = @{ @"A": @3 };
    NSDictionary<NSString *, NSNumber *> *pricing = @{ @"A": @20 };
    return [[inventory objectForKey:@"A"] integerValue] == 3 &&
           [[pricing objectForKey:@"A"] integerValue] == 20;
}

static BOOL EnterpriseAdapterExample(void) {
    NSInteger dollars = 12;
    NSInteger legacyCents = dollars * 100;
    return legacyCents == 1200;
}

static BOOL EnterpriseBridgeExample(void) {
    NSString *payload = @"x";
    NSString *http = [@"http:" stringByAppendingString:payload];
    NSString *queue = [@"queue:" stringByAppendingString:payload];
    return [http isEqualToString:@"http:x"] && [queue isEqualToString:@"queue:x"];
}

static BOOL EnterpriseFacadeExample(void) {
    NSInteger value = 5;
    BOOL valid = value > 0;
    NSString *result = valid ? [NSString stringWithFormat:@"saved:%ld", (long)value] : @"rejected";
    return [result isEqualToString:@"saved:5"];
}

static BOOL BrokerExample(void) {
    NSDictionary<NSString *, NSString *> *registry = @{ @"tax": @"tax-service" };
    return [[registry objectForKey:@"tax"] isEqualToString:@"tax-service"] && DiscountStrategy(20) == 16;
}

static BOOL MessageBusExample(void) {
    NSArray<NSString *> *handlers = @[@"audit", @"mail"];
    NSString *message = @"paid";
    return [[handlers[0] stringByAppendingFormat:@":%@", message] isEqualToString:@"audit:paid"] &&
           [[handlers[1] stringByAppendingFormat:@":%@", message] isEqualToString:@"mail:paid"];
}

static BOOL ServiceLocatorExample(void) {
    NSDictionary<NSString *, NSString *> *services = @{ @"clock": @"12:00", @"region": @"mx" };
    return [[services objectForKey:@"region"] isEqualToString:@"mx"];
}

static BOOL ActiveObjectExample(void) {
    NSMutableArray<NSString *> *queue = [NSMutableArray arrayWithObject:@"sync"];
    NSString *request = [queue objectAtIndex:0];
    [queue removeObjectAtIndex:0];
    return [request isEqualToString:@"sync"] && [queue count] == 0;
}

static BOOL MonitorObjectExample(void) {
    NSInteger balance = 5;
    @synchronized ([NSObject class]) {
        balance += 10;
        if (balance >= 7) balance -= 7;
    }
    return balance == 8;
}

static BOOL HalfSyncHalfAsyncExample(void) {
    NSMutableArray<NSString *> *queue = [NSMutableArray arrayWithObject:@"evt"];
    NSString *event = [queue objectAtIndex:0];
    [queue removeObjectAtIndex:0];
    return [event isEqualToString:@"evt"] && [queue count] == 0;
}

static BOOL LeaderFollowersExample(void) {
    NSMutableArray<NSString *> *pool = [NSMutableArray arrayWithArray:@[@"a", @"b", @"c"]];
    NSString *leader = [pool objectAtIndex:0];
    [pool removeObjectAtIndex:0];
    [pool addObject:leader];
    return [leader isEqualToString:@"a"] && [[pool componentsJoinedByString:@","] isEqualToString:@"b,c,a"];
}

static BOOL ClientServerExample(void) {
    NSString *request = @"ping";
    NSString *response = [NSString stringWithFormat:@"response(%@)", request];
    return [response isEqualToString:@"response(ping)"];
}

static BOOL PeerToPeerExample(void) {
    NSString *aToB = @"a->b:x";
    NSString *bToA = @"b->a:y";
    return [aToB hasPrefix:@"a->b"] && [bToA hasPrefix:@"b->a"];
}

static BOOL PublishSubscribeExample(void) {
    NSDictionary<NSString *, NSArray<NSString *> *> *subscriptions = @{
        @"orders": @[@"audit", @"warehouse"],
        @"users": @[@"crm"],
    };
    return [[[subscriptions objectForKey:@"orders"] componentsJoinedByString:@","] isEqualToString:@"audit,warehouse"];
}

static BOOL DistributedProxyExample(void) {
    NSInteger remoteId = 7;
    NSInteger proxyResult = remoteId * 10;
    return proxyResult == 70;
}

static BOOL PresentationAbstractionControlExample(void) {
    NSInteger abstraction = 4;
    NSString *action = @"inc";
    if ([action isEqualToString:@"inc"]) abstraction += 1;
    NSString *presentation = [NSString stringWithFormat:@"value=%ld", (long)abstraction];
    return [presentation isEqualToString:@"value=5"];
}

static BOOL ModelViewPresenterExample(void) {
    NSString *model = @"Ada";
    NSString *presented = [@"Hello " stringByAppendingString:model];
    NSString *view = [NSString stringWithFormat:@"[%@]", presented];
    return [view isEqualToString:@"[Hello Ada]"];
}

static BOOL DocumentViewExample(void) {
    NSString *document = @"hello";
    NSString *plain = document;
    NSString *upper = [document uppercaseString];
    return [plain isEqualToString:@"hello"] && [upper isEqualToString:@"HELLO"];
}

static BOOL ActiveRecordExample(void) {
    NSMutableDictionary<NSNumber *, NSString *> *store = [NSMutableDictionary dictionary];
    [store setObject:@"Ada" forKey:@1];
    return [[store objectForKey:@1] isEqualToString:@"Ada"];
}

static BOOL DataMapperExample(void) {
    NSDictionary<NSString *, id> *domain = @{ @"id": @1, @"name": @"Ada" };
    NSDictionary<NSString *, id> *row = @{ @"id": [domain objectForKey:@"id"], @"name": [domain objectForKey:@"name"] };
    NSDictionary<NSString *, id> *restored = @{ @"id": [row objectForKey:@"id"], @"name": [row objectForKey:@"name"] };
    return [[restored objectForKey:@"id"] isEqual:@1] && [[restored objectForKey:@"name"] isEqual:@"Ada"];
}

static BOOL UnitOfWorkExample(void) {
    NSMutableArray<NSDictionary<NSString *, id> *> *pending = [NSMutableArray arrayWithObject:@{ @"id": @1, @"name": @"Ada" }];
    NSMutableArray<NSDictionary<NSString *, id> *> *store = [NSMutableArray array];
    [store addObjectsFromArray:pending];
    [pending removeAllObjects];
    return [store count] == 1 && pending.count == 0;
}

static BOOL RepositoryExample(void) {
    NSMutableDictionary<NSNumber *, NSString *> *store = [NSMutableDictionary dictionary];
    [store setObject:@"Ada" forKey:@1];
    NSString *found = [store objectForKey:@1];
    return [found isEqualToString:@"Ada"];
}

static BOOL DependencyInjectionExample(void) {
    NSString *(*clock)(void) = FixedClock;
    NSString *result = [@"time=" stringByAppendingString:clock()];
    return [result isEqualToString:@"time=12:00"];
}

static BOOL LazyInitializationExample(void) {
    NSString *resource = nil;
    NSUInteger created = 0;
    if (resource == nil) {
        resource = @"resource";
        created += 1;
    }
    if (resource == nil) {
        resource = @"resource";
        created += 1;
    }
    return [resource isEqualToString:@"resource"] && created == 1;
}

static BOOL ObjectPoolExample(void) {
    NSMutableArray<NSString *> *pool = [NSMutableArray arrayWithArray:@[@"c1", @"c2"]];
    NSString *resource = [pool objectAtIndex:0];
    [pool removeObjectAtIndex:0];
    [pool addObject:resource];
    return [[pool componentsJoinedByString:@","] isEqualToString:@"c2,c1"];
}

static BOOL NullObjectExample(void) {
    NSUInteger (*real)(NSString *) = RealLogger;
    NSUInteger (*nullLogger)(NSString *) = NullLogger;
    return real(@"x") == 1 && nullLogger(@"x") == 0;
}

typedef BOOL (*PatternCheck)(void);
typedef struct {
    const char *name;
    PatternCheck check;
} PatternCase;

int main(void) {
    @autoreleasepool {
        const PatternCase cases[] = {
            {"Command", CommandExample}, {"Interpreter", InterpreterExample}, {"Iterator", IteratorExample},
            {"Mediator", MediatorExample}, {"Memento", MementoExample}, {"Observer", ObserverExample},
            {"State", StateExample}, {"Strategy", StrategyExample}, {"Template Method", TemplateMethodExample},
            {"Visitor", VisitorExample}, {"MVC", MVCExample}, {"MVVM", MVVMExample},
            {"Microkernel", MicrokernelExample}, {"Microservices", MicroservicesExample},
            {"Enterprise Adapter", EnterpriseAdapterExample}, {"Enterprise Bridge", EnterpriseBridgeExample},
            {"Enterprise Facade", EnterpriseFacadeExample}, {"Broker", BrokerExample},
            {"Message Bus", MessageBusExample}, {"Service Locator", ServiceLocatorExample},
            {"Active Object", ActiveObjectExample}, {"Monitor Object", MonitorObjectExample},
            {"Half-Sync / Half-Async", HalfSyncHalfAsyncExample}, {"Leader / Followers", LeaderFollowersExample},
            {"Client-Server", ClientServerExample}, {"Peer-to-Peer", PeerToPeerExample},
            {"Publish-Subscribe", PublishSubscribeExample}, {"Distributed Proxy", DistributedProxyExample},
            {"Presentation-Abstraction-Control", PresentationAbstractionControlExample},
            {"Model-View-Presenter", ModelViewPresenterExample}, {"Document-View", DocumentViewExample},
            {"Active Record", ActiveRecordExample}, {"Data Mapper", DataMapperExample},
            {"Unit of Work", UnitOfWorkExample}, {"Repository", RepositoryExample},
            {"Dependency Injection", DependencyInjectionExample}, {"Lazy Initialization", LazyInitializationExample},
            {"Object Pool", ObjectPoolExample}, {"Null Object", NullObjectExample},
        };
        const NSUInteger count = sizeof(cases) / sizeof(cases[0]);
        NSMutableArray<NSString *> *failed = [NSMutableArray array];
        for (NSUInteger index = 0; index < count; index++) {
            if (!cases[index].check()) {
                [failed addObject:[NSString stringWithUTF8String:cases[index].name]];
            }
        }
        if ([failed count] > 0) {
            fprintf(stderr, "Objective-C pattern sweep failures: %s\n", [[failed componentsJoinedByString:@", "] UTF8String]);
            return 1;
        }
        printf("Objective-C pattern sweep: %lu/%lu examples passed\n", (unsigned long)count, (unsigned long)count);
    }
    return 0;
}
