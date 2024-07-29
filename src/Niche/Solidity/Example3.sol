// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// Abstract Product
abstract contract Report {
    function generate() public virtual view returns (string memory);
}

// Concrete Product
contract PDFReport is Report {
    function generate() public pure override returns (string memory) {
        return "Generating PDF report";
    }
}

contract HTMLReport is Report {
    function generate() public pure override returns (string memory) {
        return "Generating HTML report";
    }
}

// Abstract Factory
abstract contract ReportFactory {
    function createReport() public virtual returns (Report);
}

// Concrete Factory
contract PDFReportFactory is ReportFactory {
    function createReport() public override returns (Report) {
        return new PDFReport();
    }
}

contract HTMLReportFactory is ReportFactory {
    function createReport() public override returns (Report) {
        return new HTMLReport();
    }
}

contract Example3 {
    function useFactory(ReportFactory factory) public view returns (string memory) {
        Report report = factory.createReport();
        return report.generate();
    }

    function test() public view returns (string memory, string memory) {
        ReportFactory pdfFactory = new PDFReportFactory();
        ReportFactory htmlFactory = new HTMLReportFactory();
        return (useFactory(pdfFactory), useFactory(htmlFactory));
    }
}
