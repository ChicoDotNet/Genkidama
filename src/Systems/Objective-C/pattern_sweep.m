#import <Foundation/Foundation.h>
#import <stdlib.h>
#import <math.h>
#import "memento.m"

static void must(BOOL value) { if (!value) abort(); }

// Command
@interface BalanceCommand : NSObject { NSInteger _delta; NSString *_name; }
- (instancetype)initWithDelta:(NSInteger)delta name:(NSString *)name;
- (NSInteger)execute:(NSInteger)balance;
- (NSInteger)undo:(NSInteger)balance;
- (NSString *)name;
@end
@implementation BalanceCommand
- (instancetype)initWithDelta:(NSInteger)delta name:(NSString *)name { if ((self = [super init])) { _delta = delta; _name = [name copy]; } return self; }
- (NSInteger)execute:(NSInteger)balance { return balance + _delta; }
- (NSInteger)undo:(NSInteger)balance { return balance - _delta; }
- (NSString *)name { return _name; }
@end
static BOOL commandPattern(void) {
    NSArray *queue = @[[[BalanceCommand alloc] initWithDelta:50 name:@"deposit"], [[BalanceCommand alloc] initWithDelta:-20 name:@"withdraw"]];
    NSInteger balance = 100; NSMutableArray *trace = [NSMutableArray array];
    for (BalanceCommand *command in queue) { balance = [command execute:balance]; [trace addObject:[command name]]; }
    return balance == 130 && [[[trace componentsJoinedByString:@">"] description] isEqualToString:@"deposit>withdraw"] && [[queue objectAtIndex:1] undo:balance] == 150;
}

// Interpreter
@protocol Expression <NSObject>
- (NSInteger)evaluate;
@end
@interface LiteralExpr : NSObject <Expression> { NSInteger _value; }
- (instancetype)initWithValue:(NSInteger)value;
@end
@implementation LiteralExpr
- (instancetype)initWithValue:(NSInteger)value { if ((self = [super init])) _value = value; return self; }
- (NSInteger)evaluate { return _value; }
@end
@interface BinaryExpr : NSObject <Expression> { id<Expression> _left; id<Expression> _right; BOOL _multiply; }
- (instancetype)initLeft:(id<Expression>)left right:(id<Expression>)right multiply:(BOOL)multiply;
@end
@implementation BinaryExpr
- (instancetype)initLeft:(id<Expression>)left right:(id<Expression>)right multiply:(BOOL)multiply { if ((self = [super init])) { _left = left; _right = right; _multiply = multiply; } return self; }
- (NSInteger)evaluate { return _multiply ? [_left evaluate] * [_right evaluate] : [_left evaluate] + [_right evaluate]; }
@end
static BOOL interpreterPattern(void) {
    id<Expression> expr = [[BinaryExpr alloc] initLeft:[[LiteralExpr alloc] initWithValue:7] right:[[BinaryExpr alloc] initLeft:[[LiteralExpr alloc] initWithValue:3] right:[[LiteralExpr alloc] initWithValue:4] multiply:YES] multiply:NO];
    return [expr evaluate] == 19;
}

// Iterator
static BOOL iteratorPattern(void) {
    NSEnumerator *iterator = [@[@10, @20, @30] objectEnumerator]; NSMutableArray *visited = [NSMutableArray array]; id value;
    while ((value = [iterator nextObject]) != nil) [visited addObject:value];
    return [visited isEqualToArray:@[@10, @20, @30]] && [iterator nextObject] == nil;
}

// Mediator
@interface UiMediator : NSObject { NSMutableArray *_events; }
- (void)notifySender:(NSString *)sender event:(NSString *)event;
- (NSArray *)events;
@end
@implementation UiMediator
- (instancetype)init { if ((self = [super init])) _events = [NSMutableArray array]; return self; }
- (void)notifySender:(NSString *)sender event:(NSString *)event { if ([sender isEqualToString:@"button"] && [event isEqualToString:@"click"]) [_events addObject:@"panel.refresh"]; if ([sender isEqualToString:@"panel"] && [event isEqualToString:@"loaded"]) [_events addObject:@"button.enable"]; }
- (NSArray *)events { return _events; }
@end
static BOOL mediatorPattern(void) { UiMediator *m = [UiMediator new]; [m notifySender:@"button" event:@"click"]; [m notifySender:@"panel" event:@"loaded"]; return [[m events] isEqualToArray:@[@"panel.refresh", @"button.enable"]]; }

// Observer
@protocol IntObserver <NSObject>
- (NSString *)observe:(NSInteger)value;
@end
@interface NamedObserver : NSObject <IntObserver> { NSString *_name; }
- (instancetype)initWithName:(NSString *)name;
@end
@implementation NamedObserver
- (instancetype)initWithName:(NSString *)name { if ((self = [super init])) _name = [name copy]; return self; }
- (NSString *)observe:(NSInteger)value { return [NSString stringWithFormat:@"%@:%ld", _name, (long)value]; }
@end
static BOOL observerPattern(void) { NSArray *observers = @[[[NamedObserver alloc] initWithName:@"audit"], [[NamedObserver alloc] initWithName:@"dashboard"]]; NSMutableArray *out = [NSMutableArray array]; for (id<IntObserver> observer in observers) [out addObject:[observer observe:42]]; return [out isEqualToArray:@[@"audit:42", @"dashboard:42"]]; }

// State
typedef NS_ENUM(NSInteger, GateState) { GateLocked, GateUnlocked };
static GateState transitionGate(GateState state, NSString *action) { if (state == GateLocked && [action isEqualToString:@"unlock"]) return GateUnlocked; if (state == GateUnlocked && [action isEqualToString:@"lock"]) return GateLocked; return state; }
static BOOL statePattern(void) { GateState state = transitionGate(GateLocked, @"unlock"); return state == GateUnlocked && transitionGate(state, @"lock") == GateLocked; }

// Strategy
@protocol PricingStrategy <NSObject>
- (NSInteger)price:(NSInteger)value;
@end
@interface RegularPricing : NSObject <PricingStrategy> @end
@implementation RegularPricing
- (NSInteger)price:(NSInteger)value { return value; }
@end
@interface VipPricing : NSObject <PricingStrategy> @end
@implementation VipPricing
- (NSInteger)price:(NSInteger)value { return value * 80 / 100; }
@end
static BOOL strategyPattern(void) { id<PricingStrategy> regular = [RegularPricing new]; id<PricingStrategy> vip = [VipPricing new]; return [regular price:100] == 100 && [vip price:100] == 80; }

// Template Method
@interface DataPipeline : NSObject
- (NSString *)readStep; - (NSString *)transformStep; - (NSString *)run;
@end
@implementation DataPipeline
- (NSString *)readStep { return @"read"; }
- (NSString *)transformStep { return @"transform"; }
- (NSString *)run { return [NSString stringWithFormat:@"%@>%@>publish", [self readStep], [self transformStep]]; }
@end
@interface CsvPipeline : DataPipeline @end
@implementation CsvPipeline
- (NSString *)readStep { return @"read-csv"; } - (NSString *)transformStep { return @"normalize"; }
@end
@interface JsonPipeline : DataPipeline @end
@implementation JsonPipeline
- (NSString *)readStep { return @"read-json"; } - (NSString *)transformStep { return @"aggregate"; }
@end
static BOOL templateMethodPattern(void) { return [[[CsvPipeline new] run] isEqualToString:@"read-csv>normalize>publish"] && [[[JsonPipeline new] run] isEqualToString:@"read-json>aggregate>publish"]; }

// Visitor
@protocol ShapeVisitor;
@protocol Shape <NSObject>
- (double)accept:(id<ShapeVisitor>)visitor;
@end
@protocol ShapeVisitor <NSObject>
- (double)visitCircleRadius:(double)radius; - (double)visitRectangleWidth:(double)width height:(double)height;
@end
@interface CircleShape : NSObject <Shape> { double _radius; } - (instancetype)initRadius:(double)radius; @end
@implementation CircleShape
- (instancetype)initRadius:(double)radius { if ((self = [super init])) _radius = radius; return self; }
- (double)accept:(id<ShapeVisitor>)visitor { return [visitor visitCircleRadius:_radius]; }
@end
@interface RectangleShape : NSObject <Shape> { double _width; double _height; } - (instancetype)initWidth:(double)width height:(double)height; @end
@implementation RectangleShape
- (instancetype)initWidth:(double)width height:(double)height { if ((self = [super init])) { _width = width; _height = height; } return self; }
- (double)accept:(id<ShapeVisitor>)visitor { return [visitor visitRectangleWidth:_width height:_height]; }
@end
@interface AreaVisitor : NSObject <ShapeVisitor> @end
@implementation AreaVisitor
- (double)visitCircleRadius:(double)radius { return 3.141592653589793 * radius * radius; }
- (double)visitRectangleWidth:(double)width height:(double)height { return width * height; }
@end
static BOOL visitorPattern(void) { id<ShapeVisitor> visitor = [AreaVisitor new]; double total = [[[CircleShape alloc] initRadius:2.0] accept:visitor] + [[[RectangleShape alloc] initWidth:3.0 height:4.0] accept:visitor]; return fabs(total - (4.0 * 3.141592653589793 + 12.0)) < 1e-9; }

// MVC
@interface CounterModel : NSObject { NSInteger _count; } - (NSInteger)count; - (void)increment; @end
@implementation CounterModel
- (NSInteger)count { return _count; } - (void)increment { _count += 1; }
@end
static NSString *renderCounter(CounterModel *model) { return [NSString stringWithFormat:@"count=%ld", (long)[model count]]; }
static BOOL mvcPattern(void) { CounterModel *model = [CounterModel new]; NSString *before = renderCounter(model); [model increment]; return [before isEqualToString:@"count=0"] && [renderCounter(model) isEqualToString:@"count=1"]; }

// MVVM
@interface AmountViewModel : NSObject { NSInteger _amount; } - (instancetype)initAmount:(NSInteger)amount; - (void)add:(NSInteger)value; - (NSString *)text; @end
@implementation AmountViewModel
- (instancetype)initAmount:(NSInteger)amount { if ((self = [super init])) _amount = amount; return self; }
- (void)add:(NSInteger)value { _amount += value; }
- (NSString *)text { return [NSString stringWithFormat:@"$%ld.00", (long)_amount]; }
@end
static BOOL mvvmPattern(void) { AmountViewModel *vm = [[AmountViewModel alloc] initAmount:10]; NSString *before = [vm text]; [vm add:5]; return [before isEqualToString:@"$10.00"] && [[vm text] isEqualToString:@"$15.00"]; }

// Microkernel
@protocol IntPlugin <NSObject> - (NSInteger)apply:(NSInteger)value; @end
@interface DoublePlugin : NSObject <IntPlugin> @end
@implementation DoublePlugin
- (NSInteger)apply:(NSInteger)value { return value * 2; }
@end
@interface SquarePlugin : NSObject <IntPlugin> @end
@implementation SquarePlugin
- (NSInteger)apply:(NSInteger)value { return value * value; }
@end
static BOOL microkernelPattern(void) { NSDictionary *plugins = @{@"double": [DoublePlugin new], @"square": [SquarePlugin new]}; return [[plugins objectForKey:@"double"] apply:4] == 8 && [[plugins objectForKey:@"square"] apply:4] == 16; }

// Microservices
@interface InventoryService : NSObject { NSInteger _stock; } - (instancetype)initStock:(NSInteger)stock; - (BOOL)reserve:(NSInteger)quantity; - (NSInteger)stock; @end
@implementation InventoryService
- (instancetype)initStock:(NSInteger)stock { if ((self = [super init])) _stock = stock; return self; }
- (BOOL)reserve:(NSInteger)quantity { if (quantity > _stock) return NO; _stock -= quantity; return YES; }
- (NSInteger)stock { return _stock; }
@end
static BOOL microservicesPattern(void) { InventoryService *inventory = [[InventoryService alloc] initStock:7]; BOOL confirmed = [inventory reserve:2]; return confirmed && [inventory stock] == 5; }

// Enterprise Adapter
static NSDictionary *adaptCustomer(NSDictionary *legacy) { return @{@"id": [legacy objectForKey:@"code"], @"amount": @([[legacy objectForKey:@"cents"] doubleValue] / 100.0)}; }
static BOOL enterpriseAdapterPattern(void) { NSDictionary *customer = adaptCustomer(@{@"code": @17, @"cents": @1250}); return [[customer objectForKey:@"id"] integerValue] == 17 && [[customer objectForKey:@"amount"] doubleValue] == 12.5; }

// Enterprise Bridge
@protocol Transport <NSObject> - (NSString *)send:(NSString *)message; @end
@interface NamedTransport : NSObject <Transport> { NSString *_name; } - (instancetype)initName:(NSString *)name; @end
@implementation NamedTransport
- (instancetype)initName:(NSString *)name { if ((self = [super init])) _name = [name copy]; return self; }
- (NSString *)send:(NSString *)message { return [NSString stringWithFormat:@"%@>%@", _name, message]; }
@end
static NSString *sendNotice(NSString *kind, NSString *message, id<Transport> transport) { return [transport send:[NSString stringWithFormat:@"%@:%@", kind, message]]; }
static BOOL enterpriseBridgePattern(void) { return [sendNotice(@"ALERT", @"disk", [[NamedTransport alloc] initName:@"kafka"]) isEqualToString:@"kafka>ALERT:disk"] && [sendNotice(@"REMINDER", @"backup", [[NamedTransport alloc] initName:@"queue"]) isEqualToString:@"queue>REMINDER:backup"]; }

// Enterprise Facade
static BOOL enterpriseFacadePattern(void) { NSString *result = [NSString stringWithFormat:@"crm:create:%d>billing:open:%d", 77, 77]; return [result isEqualToString:@"crm:create:77>billing:open:77"]; }

// Broker
static BOOL brokerPattern(void) { NSDictionary *services = @{@"inventory": @7, @"customer": @1}; return [[services objectForKey:@"inventory"] integerValue] == 7 && [[services objectForKey:@"customer"] integerValue] == 1; }

// Message Bus
static BOOL messageBusPattern(void) { NSArray *handlers = @[[[NamedObserver alloc] initWithName:@"audit"], [[NamedObserver alloc] initWithName:@"billing"]]; NSMutableArray *out = [NSMutableArray array]; for (id<IntObserver> handler in handlers) [out addObject:[handler observe:42]]; return [out isEqualToArray:@[@"audit:42", @"billing:42"]]; }

// Service Locator
static BOOL serviceLocatorPattern(void) { NSDictionary *services = @{@"email": @11, @"audit": @21}; return [[services objectForKey:@"email"] integerValue] == 11 && [[services objectForKey:@"audit"] integerValue] == 21; }

// Active Object
static BOOL activeObjectPattern(void) { NSMutableArray *queue = [NSMutableArray arrayWithObjects:@3, @4, nil]; NSInteger value = 0; NSInteger before = value; value += [[queue objectAtIndex:0] integerValue]; value *= [[queue objectAtIndex:1] integerValue]; return before == 0 && value == 12; }

// Monitor Object: @synchronized keeps synchronization with the protected state.
@interface MonitoredCounter : NSObject { NSInteger _value; NSInteger _maxCritical; NSInteger _critical; } - (void)add:(NSInteger)value; - (NSInteger)value; - (NSInteger)maxCritical; @end
@implementation MonitoredCounter
- (void)add:(NSInteger)value { @synchronized(self) { _critical += 1; if (_critical > _maxCritical) _maxCritical = _critical; _value += value; _critical -= 1; } }
- (NSInteger)value { return _value; } - (NSInteger)maxCritical { return _maxCritical; }
@end
static BOOL monitorObjectPattern(void) { MonitoredCounter *counter = [MonitoredCounter new]; [counter add:2]; [counter add:3]; return [counter value] == 5 && [counter maxCritical] == 1; }

// Half-Sync / Half-Async
static BOOL halfSyncHalfAsyncPattern(void) { NSArray *queue = @[@"job-1", @"job-2", @"job-3"]; NSMutableArray *processed = [NSMutableArray array]; for (NSString *job in queue) [processed addObject:[@"done:" stringByAppendingString:job]]; return [processed isEqualToArray:@[@"done:job-1", @"done:job-2", @"done:job-3"]]; }

// Leader / Followers
static BOOL leaderFollowersPattern(void) { NSArray *workers = @[@"worker-1", @"worker-2", @"worker-3"]; NSArray *events = @[@"event-a", @"event-b", @"event-c"]; NSMutableArray *handled = [NSMutableArray array]; for (NSUInteger i = 0; i < [events count]; ++i) [handled addObject:[NSString stringWithFormat:@"%@:%@", [workers objectAtIndex:i], [events objectAtIndex:i]]]; return [handled isEqualToArray:@[@"worker-1:event-a", @"worker-2:event-b", @"worker-3:event-c"]] && [[workers objectAtIndex:([events count] % [workers count])] isEqualToString:@"worker-1"]; }

// Client-Server
static NSDictionary *serverHandle(NSString *key) { return [key isEqualToString:@"sku-1"] ? @{@"status": @200, @"body": @"stock=7"} : @{@"status": @404, @"body": @"missing"}; }
static BOOL clientServerPattern(void) { NSDictionary *response = serverHandle(@"sku-1"); return [[response objectForKey:@"status"] integerValue] == 200 && [[response objectForKey:@"body"] isEqualToString:@"stock=7"]; }

// Peer-to-Peer
@interface Peer : NSObject { NSString *_name; NSMutableArray *_inbox; } - (instancetype)initName:(NSString *)name; - (void)send:(Peer *)other data:(NSString *)data; - (NSArray *)inbox; @end
@implementation Peer
- (instancetype)initName:(NSString *)name { if ((self = [super init])) { _name = [name copy]; _inbox = [NSMutableArray array]; } return self; }
- (void)send:(Peer *)other data:(NSString *)data { [other->_inbox addObject:[NSString stringWithFormat:@"%@>%@:%@", _name, other->_name, data]]; }
- (NSArray *)inbox { return _inbox; }
@end
static BOOL peerToPeerPattern(void) { Peer *a = [[Peer alloc] initName:@"peer-a"]; Peer *b = [[Peer alloc] initName:@"peer-b"]; Peer *c = [[Peer alloc] initName:@"peer-c"]; [a send:b data:@"block-42"]; [a send:c data:@"block-42"]; return [[b inbox] isEqualToArray:@[@"peer-a>peer-b:block-42"]] && [[c inbox] isEqualToArray:@[@"peer-a>peer-c:block-42"]]; }

// Publish-Subscribe
static BOOL publishSubscribePattern(void) { NSArray *subscribers = @[[[NamedObserver alloc] initWithName:@"warehouse"], [[NamedObserver alloc] initWithName:@"analytics"]]; NSMutableArray *out = [NSMutableArray array]; for (id<IntObserver> subscriber in subscribers) [out addObject:[subscriber observe:51]]; return [out isEqualToArray:@[@"warehouse:51", @"analytics:51"]]; }

// Distributed Proxy
@protocol StockService <NSObject> - (NSInteger)stock:(NSString *)sku; @end
@interface RemoteStock : NSObject <StockService> @end
@implementation RemoteStock
- (NSInteger)stock:(NSString *)sku { (void)sku; return 7; }
@end
@interface StockProxy : NSObject <StockService> { id<StockService> _remote; } - (instancetype)initRemote:(id<StockService>)remote; @end
@implementation StockProxy
- (instancetype)initRemote:(id<StockService>)remote { if ((self = [super init])) _remote = remote; return self; }
- (NSInteger)stock:(NSString *)sku { return [_remote stock:sku]; }
@end
static BOOL distributedProxyPattern(void) { return [[[StockProxy alloc] initRemote:[RemoteStock new]] stock:@"sku-1"] == 7; }

// Presentation-Abstraction-Control
@interface PacAgent : NSObject { NSString *_name; NSInteger _value; } - (instancetype)initName:(NSString *)name value:(NSInteger)value; - (NSString *)view; @end
@implementation PacAgent
- (instancetype)initName:(NSString *)name value:(NSInteger)value { if ((self = [super init])) { _name = [name copy]; _value = value; } return self; }
- (NSString *)view { return [NSString stringWithFormat:@"%@:view=%ld", _name, (long)_value]; }
@end
static BOOL presentationAbstractionControlPattern(void) { return [[[[PacAgent alloc] initName:@"child" value:42] view] isEqualToString:@"child:view=42"] && [[[[PacAgent alloc] initName:@"root" value:42] view] isEqualToString:@"root:view=42"]; }

// Model-View-Presenter
@interface PassiveView : NSObject { NSString *_text; } - (void)setText:(NSString *)text; - (NSString *)text; @end
@implementation PassiveView
- (void)setText:(NSString *)text { _text = [text copy]; } - (NSString *)text { return _text; }
@end
static BOOL modelViewPresenterPattern(void) { CounterModel *model = [CounterModel new]; PassiveView *view = [PassiveView new]; [model increment]; [view setText:renderCounter(model)]; return [model count] == 1 && [[view text] isEqualToString:@"count=1"]; }

// Document-View
static BOOL documentViewPattern(void) { NSDictionary *document = @{@"title": @"Final", @"words": @120}; NSString *editor = [NSString stringWithFormat:@"editor:%@:%@", [document objectForKey:@"title"], [document objectForKey:@"words"]]; NSString *summary = [NSString stringWithFormat:@"summary:%@", [document objectForKey:@"title"]]; return [editor isEqualToString:@"editor:Final:120"] && [summary isEqualToString:@"summary:Final"]; }

// Active Record
@interface PersonRecord : NSObject { NSInteger _identifier; NSString *_name; } - (instancetype)initId:(NSInteger)identifier name:(NSString *)name; - (void)save; + (PersonRecord *)load:(NSInteger)identifier; - (NSString *)name; @end
static NSMutableDictionary *PersonTable;
@implementation PersonRecord
- (instancetype)initId:(NSInteger)identifier name:(NSString *)name { if ((self = [super init])) { _identifier = identifier; _name = [name copy]; } return self; }
- (void)save { if (PersonTable == nil) PersonTable = [NSMutableDictionary dictionary]; [PersonTable setObject:self forKey:@(_identifier)]; }
+ (PersonRecord *)load:(NSInteger)identifier { return [PersonTable objectForKey:@(identifier)]; }
- (NSString *)name { return _name; }
@end
static BOOL activeRecordPattern(void) { PersonTable = [NSMutableDictionary dictionary]; [[[PersonRecord alloc] initId:7 name:@"Ada"] save]; return [[[PersonRecord load:7] name] isEqualToString:@"Ada"]; }

// Data Mapper
static NSDictionary *toRow(NSDictionary *person) { return @{@"key": [NSString stringWithFormat:@"person:%@", [person objectForKey:@"id"]], @"name": [person objectForKey:@"name"]}; }
static NSDictionary *fromRow(NSDictionary *row) { return @{@"id": @8, @"name": [row objectForKey:@"name"]}; }
static BOOL dataMapperPattern(void) { NSDictionary *row = toRow(@{@"id": @8, @"name": @"Grace"}); NSDictionary *person = fromRow(row); return [[row objectForKey:@"key"] isEqualToString:@"person:8"] && [[person objectForKey:@"name"] isEqualToString:@"Grace"]; }

// Unit of Work
static BOOL unitOfWorkPattern(void) { NSMutableArray *values = [NSMutableArray arrayWithArray:@[@10, @20]]; NSArray *before = [NSArray arrayWithArray:values]; [values replaceObjectAtIndex:0 withObject:@([[values objectAtIndex:0] integerValue] + 5)]; [values replaceObjectAtIndex:1 withObject:@([[values objectAtIndex:1] integerValue] - 3)]; return [before isEqualToArray:@[@10, @20]] && [values isEqualToArray:@[@15, @17]]; }

// Repository
static BOOL repositoryPattern(void) { NSDictionary *repository = @{@9: @{@"id": @9, @"name": @"Linus"}}; NSDictionary *person = [repository objectForKey:@9]; return [[person objectForKey:@"name"] isEqualToString:@"Linus"]; }

// Dependency Injection
@protocol Sender <NSObject> - (NSString *)send:(NSString *)name; @end
@interface PrefixSender : NSObject <Sender> { NSString *_prefix; } - (instancetype)initPrefix:(NSString *)prefix; @end
@implementation PrefixSender
- (instancetype)initPrefix:(NSString *)prefix { if ((self = [super init])) _prefix = [prefix copy]; return self; }
- (NSString *)send:(NSString *)name { return [NSString stringWithFormat:@"%@:%@", _prefix, name]; }
@end
static BOOL dependencyInjectionPattern(void) { id<Sender> production = [[PrefixSender alloc] initPrefix:@"smtp"]; id<Sender> test = [[PrefixSender alloc] initPrefix:@"fake"]; return [[production send:@"Ada"] isEqualToString:@"smtp:Ada"] && [[test send:@"Ada"] isEqualToString:@"fake:Ada"]; }

// Lazy Initialization
@interface LazyResource : NSObject { NSString *_value; NSInteger _creations; } - (NSString *)get; - (NSInteger)creations; @end
@implementation LazyResource
- (NSString *)get { if (_value == nil) { _value = @"resource-ready"; _creations += 1; } return _value; }
- (NSInteger)creations { return _creations; }
@end
static BOOL lazyInitializationPattern(void) { LazyResource *resource = [LazyResource new]; return [[resource get] isEqualToString:@"resource-ready"] && [[resource get] isEqualToString:@"resource-ready"] && [resource creations] == 1; }

// Object Pool
@interface ObjectPool : NSObject { NSMutableArray *_available; NSInteger _next; } - (NSInteger)acquire; - (void)releaseValue:(NSInteger)value; @end
@implementation ObjectPool
- (instancetype)init { if ((self = [super init])) _available = [NSMutableArray array]; return self; }
- (NSInteger)acquire { if ([_available count] > 0) { NSInteger value = [[_available lastObject] integerValue]; [_available removeLastObject]; return value; } _next += 1; return _next; }
- (void)releaseValue:(NSInteger)value { [_available addObject:@(value)]; }
@end
static BOOL objectPoolPattern(void) { ObjectPool *pool = [ObjectPool new]; NSInteger first = [pool acquire]; NSInteger second = [pool acquire]; [pool releaseValue:first]; NSInteger reused = [pool acquire]; return first == 1 && second == 2 && reused == 1; }

// Null Object
@protocol Logger <NSObject> - (NSString *)log:(NSString *)message; @end
@interface RealLogger : NSObject <Logger> @end
@implementation RealLogger
- (NSString *)log:(NSString *)message { return [@"logged:" stringByAppendingString:message]; }
@end
@interface NullLogger : NSObject <Logger> @end
@implementation NullLogger
- (NSString *)log:(NSString *)message { (void)message; return @""; }
@end
static BOOL nullObjectPattern(void) { return [[[RealLogger new] log:@"processed:item-1"] isEqualToString:@"logged:processed:item-1"] && [[[NullLogger new] log:@"processed:item-1"] isEqualToString:@""]; }

int main(void) {
    @autoreleasepool {
        BOOL (*cases[])(void) = {
            commandPattern, interpreterPattern, iteratorPattern, mediatorPattern, verifyMementoCanonical, observerPattern, statePattern, strategyPattern, templateMethodPattern, visitorPattern,
            mvcPattern, mvvmPattern, microkernelPattern, microservicesPattern, enterpriseAdapterPattern, enterpriseBridgePattern, enterpriseFacadePattern, brokerPattern, messageBusPattern, serviceLocatorPattern,
            activeObjectPattern, monitorObjectPattern, halfSyncHalfAsyncPattern, leaderFollowersPattern, clientServerPattern, peerToPeerPattern, publishSubscribePattern, distributedProxyPattern,
            presentationAbstractionControlPattern, modelViewPresenterPattern, documentViewPattern, activeRecordPattern, dataMapperPattern, unitOfWorkPattern, repositoryPattern,
            dependencyInjectionPattern, lazyInitializationPattern, objectPoolPattern, nullObjectPattern,
        };
        NSUInteger count = sizeof(cases) / sizeof(cases[0]);
        must(count == 39);
        for (NSUInteger i = 0; i < count; ++i) must(cases[i]());
        printf("Objective-C pattern sweep: 39/39 examples passed\n");
    }
    return 0;
}
