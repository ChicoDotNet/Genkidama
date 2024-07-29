#import <Foundation/Foundation.h>

// Abstract Product
@protocol Report <NSObject>
- (void)generate;
@end

// Concrete Product
@interface PDFReport : NSObject <Report>
@end

@implementation PDFReport
- (void)generate {
    NSLog(@"Generating PDF report");
}
@end

@interface HTMLReport : NSObject <Report>
@end

@implementation HTMLReport
- (void)generate {
    NSLog(@"Generating HTML report");
}
@end

// Abstract Factory
@protocol ReportFactory <NSObject>
- (id<Report>)createReport;
@end

// Concrete Factory
@interface PDFReportFactory : NSObject <ReportFactory>
@end

@implementation PDFReportFactory
- (id<Report>)createReport {
    return [PDFReport new];
}
@end

@interface HTMLReportFactory : NSObject <ReportFactory>
@end

@implementation HTMLReportFactory
- (id<Report>)createReport {
    return [HTMLReport new];
}
@end

void useFactory(id<ReportFactory> factory) {
    id<Report> report = [factory createReport];
    [report generate];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        useFactory([PDFReportFactory new]);
        useFactory([HTMLReportFactory new]);
    }
    return 0;
}
