# Abstract Factory
function Create-Report {
    param (
        [string]$reportType
    )

    if ($reportType -eq "pdf") {
        Generate-PDFReport
    } elseif ($reportType -eq "html") {
        Generate-HTMLReport
    }
}

# Concrete Implementations
function Generate-PDFReport {
    Write-Output "Generating PDF report"
}

function Generate-HTMLReport {
    Write-Output "Generating HTML report"
}

# Usage
Create-Report -reportType "pdf"
Create-Report -reportType "html"
