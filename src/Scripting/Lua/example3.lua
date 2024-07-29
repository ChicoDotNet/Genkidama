-- Abstract Factory
ReportFactory = {}
function ReportFactory:createReport(reportType)
    if reportType == "pdf" then
        return PDFReport:new()
    elseif reportType == "html" then
        return HTMLReport:new()
    end
end

-- Concrete Products
PDFReport = {}
function PDFReport:new()
    local newObj = {}
    self.__index = self
    return setmetatable(newObj, self)
end
function PDFReport:generate()
    print("Generating PDF report")
end

HTMLReport = {}
function HTMLReport:new()
    local newObj = {}
    self.__index = self
    return setmetatable(newObj, self)
end
function HTMLReport:generate()
    print("Generating HTML report")
end

-- Usage
local reportFactory = ReportFactory
local report1 = reportFactory:createReport("pdf")
report1:generate()

local report2 = reportFactory:createReport("html")
report2:generate()
