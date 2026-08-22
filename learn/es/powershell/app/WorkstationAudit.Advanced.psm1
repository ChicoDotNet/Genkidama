Set-StrictMode -Version Latest

function Resolve-AuditTarget {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ValidateNotNullOrEmpty()][string]$ComputerName,
        [switch]$AllowRemote
    )

    $normalized = $ComputerName.Trim()
    $localNames = @('.', 'localhost', [Environment]::MachineName)
    $isLocal = $localNames -contains $normalized

    if (-not $isLocal -and -not $AllowRemote) {
        throw [System.InvalidOperationException]::new("El destino remoto '$normalized' requiere -AllowRemote. WorkstationAudit nunca habilita remoting ni cambia WinRM automáticamente.")
    }

    [pscustomobject]@{
        ComputerName = if ($isLocal) { [Environment]::MachineName } else { $normalized }
        Mode = if ($isLocal) { 'Local' } else { 'Remote' }
        RequiresRemoting = -not $isLocal
        ChangesSystemConfiguration = $false
    }
}

function Get-AuditFleetSummary {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory, ValueFromPipeline)][ValidateNotNullOrEmpty()][string[]]$Path,
        [ValidateRange(1, 32)][int]$ThrottleLimit = 4
    )

    begin { $paths = [System.Collections.Generic.List[string]]::new() }
    process { foreach ($item in $Path) { $paths.Add($item) } }
    end {
        if ($paths.Count -eq 0) { throw [System.ArgumentException]::new('Debes proporcionar al menos un reporte.') }

        $readReport = {
            param($ReportPath)
            if (-not (Test-Path -LiteralPath $ReportPath -PathType Leaf)) {
                throw [System.IO.FileNotFoundException]::new("No existe el reporte: $ReportPath")
            }
            try {
                $audit = Get-Content -LiteralPath $ReportPath -Raw -ErrorAction Stop | ConvertFrom-Json -ErrorAction Stop
            }
            catch {
                throw [System.IO.InvalidDataException]::new("No se pudo leer un reporte válido: $ReportPath", $_.Exception)
            }
            if ($null -eq $audit.SchemaVersion -or $null -eq $audit.Snapshot -or $null -eq $audit.Findings) {
                throw [System.IO.InvalidDataException]::new("El reporte no contiene el contrato mínimo de WorkstationAudit: $ReportPath")
            }
            [pscustomobject]@{
                Path = $ReportPath
                ComputerName = [string]$audit.Snapshot.ComputerName
                Findings = @($audit.Findings).Count
                Critical = @($audit.Findings | Where-Object Severity -eq 'Critical').Count
                Warning = @($audit.Findings | Where-Object Severity -eq 'Warning').Count
            }
        }

        $records = if ($ThrottleLimit -eq 1 -or $paths.Count -eq 1) {
            @($paths | ForEach-Object { & $readReport $_ })
        }
        else {
            @($paths | ForEach-Object -ThrottleLimit $ThrottleLimit -Parallel {
                $reportPath = $_
                if (-not (Test-Path -LiteralPath $reportPath -PathType Leaf)) {
                    throw [System.IO.FileNotFoundException]::new("No existe el reporte: $reportPath")
                }
                try {
                    $audit = Get-Content -LiteralPath $reportPath -Raw -ErrorAction Stop | ConvertFrom-Json -ErrorAction Stop
                }
                catch {
                    throw [System.IO.InvalidDataException]::new("No se pudo leer un reporte válido: $reportPath", $_.Exception)
                }
                if ($null -eq $audit.SchemaVersion -or $null -eq $audit.Snapshot -or $null -eq $audit.Findings) {
                    throw [System.IO.InvalidDataException]::new("El reporte no contiene el contrato mínimo de WorkstationAudit: $reportPath")
                }
                [pscustomobject]@{
                    Path = $reportPath
                    ComputerName = [string]$audit.Snapshot.ComputerName
                    Findings = @($audit.Findings).Count
                    Critical = @($audit.Findings | Where-Object Severity -eq 'Critical').Count
                    Warning = @($audit.Findings | Where-Object Severity -eq 'Warning').Count
                }
            })
        }

        $ordered = @($records | Sort-Object ComputerName, Path)
        [pscustomobject]@{
            ReportCount = $ordered.Count
            TotalFindings = ($ordered | Measure-Object -Property Findings -Sum).Sum
            Critical = ($ordered | Measure-Object -Property Critical -Sum).Sum
            Warning = ($ordered | Measure-Object -Property Warning -Sum).Sum
            ThrottleLimit = $ThrottleLimit
            Reports = $ordered
        }
    }
}

function Measure-AuditOperation {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][ValidateNotNullOrEmpty()][string]$Name,
        [Parameter(Mandatory)][scriptblock]$Operation
    )

    $watch = [System.Diagnostics.Stopwatch]::StartNew()
    try { $result = & $Operation }
    finally { $watch.Stop() }

    [pscustomobject]@{
        Name = $Name
        ElapsedMilliseconds = $watch.Elapsed.TotalMilliseconds
        Result = $result
    }
}

function Test-WorkstationAuditContract {
    [CmdletBinding()]
    param([Parameter(Mandatory, ValueFromPipeline)][psobject]$Audit)

    process {
        $issues = [System.Collections.Generic.List[string]]::new()
        if ($null -eq $Audit.PSObject.Properties['SchemaVersion']) { $issues.Add('Falta SchemaVersion.') }
        if ($null -eq $Audit.PSObject.Properties['Snapshot']) { $issues.Add('Falta Snapshot.') }
        if ($null -eq $Audit.PSObject.Properties['Findings']) { $issues.Add('Falta Findings.') }
        if ($null -ne $Audit.PSObject.Properties['Snapshot'] -and $null -eq $Audit.Snapshot.PSObject.Properties['ComputerName']) { $issues.Add('Falta Snapshot.ComputerName.') }
        if ($null -ne $Audit.PSObject.Properties['Findings']) {
            foreach ($finding in @($Audit.Findings)) {
                if ($null -eq $finding.PSObject.Properties['Code'] -or [string]::IsNullOrWhiteSpace([string]$finding.Code)) { $issues.Add('Existe un finding sin Code.') }
                if ($null -eq $finding.PSObject.Properties['Severity'] -or @('Info', 'Warning', 'Critical') -notcontains [string]$finding.Severity) { $issues.Add('Existe un finding con Severity inválida.') }
            }
        }
        [pscustomobject]@{ IsValid = $issues.Count -eq 0; Issues = @($issues) }
    }
}

function Get-WorkstationAuditExitCode {
    [CmdletBinding()]
    param([Parameter(Mandatory)][psobject]$Audit)

    if ($null -eq $Audit.Summary) { throw [System.ArgumentException]::new('La auditoría no contiene Summary.') }
    if ([int]$Audit.Summary.Critical -gt 0) { return 2 }
    if ([int]$Audit.Summary.Warning -gt 0) { return 1 }
    0
}

Export-ModuleMember -Function Resolve-AuditTarget, Get-AuditFleetSummary, Measure-AuditOperation, Test-WorkstationAuditContract, Get-WorkstationAuditExitCode
