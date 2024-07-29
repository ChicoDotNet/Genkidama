#import <Foundation/Foundation.h>

// Abstract Product
@protocol Button
- (void)render;
@end

@protocol Checkbox
- (void)render;
@end

// Concrete Product
@interface DarkButton : NSObject <Button>
@end

@implementation DarkButton
- (void)render {
    NSLog(@"Dark Button");
}
@end

@interface LightButton : NSObject <Button>
@end

@implementation LightButton
- (void)render {
    NSLog(@"Light Button");
}
@end

@interface DarkCheckbox : NSObject <Checkbox>
@end

@implementation DarkCheckbox
- (void)render {
    NSLog(@"Dark Checkbox");
}
@end

@interface LightCheckbox : NSObject <Checkbox>
@end

@implementation LightCheckbox
- (void)render {
    NSLog(@"Light Checkbox");
}
@end

// Abstract Factory
@protocol UIFactory
- (id<Button>)createButton;
- (id<Checkbox>)createCheckbox;
@end

// Concrete Factory
@interface DarkFactory : NSObject <UIFactory>
@end

@implementation DarkFactory
- (id<Button>)createButton {
    return [DarkButton new];
}
- (id<Checkbox>)createCheckbox {
    return [DarkCheckbox new];
}
@end

@interface LightFactory : NSObject <UIFactory>
@end

@implementation LightFactory
- (id<Button>)createButton {
    return [LightButton new];
}
- (id<Checkbox>)createCheckbox {
    return [LightCheckbox new];
}
@end

void createUIComponents(id<UIFactory> factory) {
    id<Button> button = [factory createButton];
    id<Checkbox> checkbox = [factory createCheckbox];
    [button render];
    [checkbox render];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        createUIComponents([DarkFactory new]);
        createUIComponents([LightFactory new]);
    }
    return 0;
}
