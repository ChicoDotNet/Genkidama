% Abstract Factory
function report = createReport(reportType)
    if strcmp(reportType, 'pdf')
        report.generate = @generatePDFReport;
    elseif strcmp(reportType, 'html')
        report.generate = @generateHTMLReport;
    end
end

% Concrete Implementations
function generatePDFReport()
    disp('Generating PDF report')
end

function generateHTMLReport()
    disp('Generating HTML report')
end

% Usage
report = createReport('pdf');
report.generate()

report = createReport('html');
report.generate()
