#import <Foundation/Foundation.h>

@protocol TemperatureReader <NSObject>
- (NSInteger)readCelsius;
@end

@interface LegacyFahrenheitSensor : NSObject
- (NSInteger)readFahrenheit;
@end

@implementation LegacyFahrenheitSensor
- (NSInteger)readFahrenheit {
    return 86;
}
@end

@interface FahrenheitSensorAdapter : NSObject <TemperatureReader>
- (instancetype)initWithSensor:(LegacyFahrenheitSensor *)sensor;
@end

@interface FahrenheitSensorAdapter ()
@property(nonatomic, strong) LegacyFahrenheitSensor *adaptee;
@end

@implementation FahrenheitSensorAdapter

- (instancetype)initWithSensor:(LegacyFahrenheitSensor *)sensor {
    self = [super init];
    if (self) {
        self.adaptee = sensor;
    }
    return self;
}

- (NSInteger)readCelsius {
    NSInteger fahrenheit = [self.adaptee readFahrenheit];
    return ((fahrenheit - 32) * 5) / 9;
}
@end

int main(void) {
    @autoreleasepool {
        LegacyFahrenheitSensor *legacy = [LegacyFahrenheitSensor new];
        id<TemperatureReader> reader = [[FahrenheitSensorAdapter alloc] initWithSensor:legacy];
        printf("legacy=%ldF\n", (long)[legacy readFahrenheit]);
        printf("adapted=%ldC\n", (long)[reader readCelsius]);
    }
    return 0;
}
