#import <Foundation/Foundation.h>

@protocol ReportBuilder <NSObject>
- (void)reset;
- (void)addTitle:(NSString *)title;
- (void)addSection:(NSString *)heading body:(NSString *)body;
- (NSString *)build;
@end

@interface TextReportBuilder : NSObject <ReportBuilder>
@property(nonatomic, strong) NSMutableArray<NSString *> *parts;
@end

@implementation TextReportBuilder
- (instancetype)init { self = [super init]; if (self) _parts = [NSMutableArray array]; return self; }
- (void)reset { [self.parts removeAllObjects]; }
- (void)addTitle:(NSString *)title { [self.parts addObject:[NSString stringWithFormat:@"# %@", title]]; }
- (void)addSection:(NSString *)heading body:(NSString *)body {
    [self.parts addObject:[NSString stringWithFormat:@"## %@", heading]];
    [self.parts addObject:body];
}
- (NSString *)build { return [self.parts componentsJoinedByString:@"\n"]; }
@end

@interface HtmlReportBuilder : NSObject <ReportBuilder>
@property(nonatomic, strong) NSMutableArray<NSString *> *parts;
@end

@implementation HtmlReportBuilder
- (instancetype)init { self = [super init]; if (self) _parts = [NSMutableArray array]; return self; }
- (void)reset { [self.parts removeAllObjects]; }
- (void)addTitle:(NSString *)title { [self.parts addObject:[NSString stringWithFormat:@"<h1>%@</h1>", title]]; }
- (void)addSection:(NSString *)heading body:(NSString *)body {
    [self.parts addObject:[NSString stringWithFormat:@"<h2>%@</h2>", heading]];
    [self.parts addObject:[NSString stringWithFormat:@"<p>%@</p>", body]];
}
- (NSString *)build { return [self.parts componentsJoinedByString:@""]; }
@end

static NSString *BuildAvailabilityReport(id<ReportBuilder> builder) {
    [builder reset];
    [builder addTitle:@"Service status"];
    [builder addSection:@"Availability" body:@"99.95%"];
    return [builder build];
}

int main(void) {
    @autoreleasepool {
        NSLog(@"%@", BuildAvailabilityReport([[TextReportBuilder alloc] init]));
        NSLog(@"---");
        NSLog(@"%@", BuildAvailabilityReport([[HtmlReportBuilder alloc] init]));
    }
    return 0;
}
