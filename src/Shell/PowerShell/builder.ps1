Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-TextReportBuilder {
    $state = [System.Collections.Generic.List[string]]::new()
    return @{
        Reset = { $state.Clear() }.GetNewClosure()
        AddTitle = { param($title) [void]$state.Add("# $title") }.GetNewClosure()
        AddSection = {
            param($heading, $body)
            [void]$state.Add("## $heading")
            [void]$state.Add($body)
        }.GetNewClosure()
        Build = { [string]::Join("`n", $state) }.GetNewClosure()
    }
}

function New-HtmlReportBuilder {
    $state = [System.Collections.Generic.List[string]]::new()
    return @{
        Reset = { $state.Clear() }.GetNewClosure()
        AddTitle = { param($title) [void]$state.Add("<h1>$title</h1>") }.GetNewClosure()
        AddSection = {
            param($heading, $body)
            [void]$state.Add("<h2>$heading</h2>")
            [void]$state.Add("<p>$body</p>")
        }.GetNewClosure()
        Build = { [string]::Join('', $state) }.GetNewClosure()
    }
}

function Build-AvailabilityReport {
    param([hashtable]$Builder)
    & $Builder.Reset
    & $Builder.AddTitle 'Service status'
    & $Builder.AddSection 'Availability' '99.95%'
    return & $Builder.Build
}

Build-AvailabilityReport (New-TextReportBuilder)
'---'
Build-AvailabilityReport (New-HtmlReportBuilder)
