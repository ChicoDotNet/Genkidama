<?php

interface Report {
    public function generate();
}

class PDFReport implements Report {
    public function generate() {
        echo "Generating PDF report\n";
    }
}

class HTMLReport implements Report {
    public function generate() {
        echo "Generating HTML report\n";
    }
}

interface ReportFactory {
    public function createReport(): Report;
}

class PDFReportFactory implements ReportFactory {
    public function createReport(): Report {
        return new PDFReport();
    }
}

class HTMLReportFactory implements ReportFactory {
    public function createReport(): Report {
        return new HTMLReport();
    }
}

function useFactory(ReportFactory $factory) {
    $report = $factory->createReport();
    $report->generate();
}

// Usage
useFactory(new PDFReportFactory());
useFactory(new HTMLReportFactory());
