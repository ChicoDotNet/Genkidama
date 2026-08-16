Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

function New-TextReportBuilder {
    $state = [System.Collections.Generic.List[string]]::new()
    return @{
        Reset = { $state.Clear() }
        AddTitle = { param($title) $state.Add("# $title") }
        AddSection = { param($heading, $body) $state.Add("## $heading"); $state.Add($body) }
        Build = { [string]::Join("`n", $state) }
    }
}

function New-HtmlReportBuilder {
    $state = [System.Collections.Generic.List[string]]::new()
    return @{
        Reset = { $state.Clear() }
        AddTitle = { param($title) $state.Add("<h1>$title</h1>") }
        AddSection = { param($heading, $body) $state.Add("<h2>$heading</h2>"); $state.Add("<p>$body</p>") }
        Build = { [string]::Concat($state) }
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
