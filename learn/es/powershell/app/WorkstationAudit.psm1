Set-StrictMode -Version Latest

function Resolve-AuditConfiguration {
    [CmdletBinding()]
    param([hashtable]$Configuration = @{})

    $warning = if ($Configuration.ContainsKey('StorageWarningPercent')) { [double]$Configuration.StorageWarningPercent } else { 20.0 }
    $critical = if ($Configuration.ContainsKey('StorageCriticalPercent')) { [double]$Configuration.StorageCriticalPercent } else { 10.0 }
    $memoryWarning = if ($Configuration.ContainsKey('MemoryWarningPercent')) { [double]$Configuration.MemoryWarningPercent } else { 15.0 }

    if ($critical -lt 0 -or $warning -le 0 -or $warning -gt 100 -or $critical -ge $warning) {
        throw [System.ArgumentException]::new('Los umbrales de almacenamiento requieren 0 <= Critical < Warning <= 100.')
    }
    if ($memoryWarning -le 0 -or $memoryWarning -gt 100) {
        throw [System.ArgumentException]::new('MemoryWarningPercent debe estar entre 0 y 100.')
    }

    [pscustomobject]@{
        StorageWarningPercent  = $warning
        StorageCriticalPercent = $critical
        MemoryWarningPercent   = $memoryWarning
    }
}

function Get-WindowsSystemSnapshot {
    [CmdletBinding()]
    param()

    if (-not $IsWindows) {
        return [pscustomobject]@{
            Supported = $false
            Platform  = 'non-windows'
            Reason    = 'Las consultas CIM de Win32 sólo se ejecutan en Windows.'
        }
    }

    try {
        $os = Get-CimInstance -ClassName Win32_OperatingSystem -ErrorAction Stop
        $computer = Get-CimInstance -ClassName Win32_ComputerSystem -ErrorAction Stop
    }
    catch {
        throw [System.InvalidOperationException]::new('No se pudieron consultar las señales CIM de Windows.', $_.Exception)
    }

    [pscustomobject]@{
        Supported            = $true
        Platform             = 'windows'
        Caption              = $os.Caption
        Version              = $os.Version
        LastBootUpTime       = $os.LastBootUpTime
        TotalMemoryBytes     = [long]$computer.TotalPhysicalMemory
        FreeMemoryBytes      = [long]$os.FreePhysicalMemory * 1KB
        Manufacturer         = $computer.Manufacturer
        Model                = $computer.Model
    }
}

function Get-PlatformSnapshot {
    [CmdletBinding()]
    param()

    $drives = @(
        Get-PSDrive -PSProvider FileSystem | ForEach-Object {
            $used = if ($null -eq $_.Used) { 0L } else { [long]$_.Used }
            $free = if ($null -eq $_.Free) { 0L } else { [long]$_.Free }
            $total = $used + $free
            [pscustomobject]@{ Name = $_.Name; Root = $_.Root; UsedBytes = $used; FreeBytes = $free; TotalBytes = $total }
        }
    )

    [pscustomobject]@{
        ComputerName      = [Environment]::MachineName
        OperatingSystem   = [System.Runtime.InteropServices.RuntimeInformation]::OSDescription
        PowerShellVersion = $PSVersionTable.PSVersion.ToString()
        CapturedAt         = [DateTimeOffset]::UtcNow
        Drives             = $drives
    }
}

function Get-StorageFinding {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory, ValueFromPipeline)][psobject]$Drive,
        [psobject]$Configuration = (Resolve-AuditConfiguration)
    )

    process {
        if ($Drive.TotalBytes -le 0) {
            return [pscustomobject]@{ Code = 'storage.unknown'; Severity = 'Info'; Message = "No hay capacidad medible para $($Drive.Name)."; Evidence = @{ Name = $Drive.Name; TotalBytes = $Drive.TotalBytes } }
        }

        $freePercent = [math]::Round(($Drive.FreeBytes / $Drive.TotalBytes) * 100, 2)
        $severity = if ($freePercent -lt $Configuration.StorageCriticalPercent) { 'Critical' } elseif ($freePercent -lt $Configuration.StorageWarningPercent) { 'Warning' } else { 'Info' }
        [pscustomobject]@{
            Code = 'storage.free-space'; Severity = $severity; Message = "La unidad $($Drive.Name) tiene $freePercent% libre."
            Evidence = @{ Name = $Drive.Name; FreeBytes = $Drive.FreeBytes; TotalBytes = $Drive.TotalBytes; FreePercent = $freePercent }
        }
    }
}

function Get-MemoryFinding {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory)][psobject]$SystemSnapshot,
        [psobject]$Configuration = (Resolve-AuditConfiguration)
    )

    if (-not $SystemSnapshot.Supported) {
        return [pscustomobject]@{ Code = 'memory.not-supported'; Severity = 'Info'; Message = 'La señal de memoria CIM no está disponible en esta plataforma.'; Evidence = @{ Platform = $SystemSnapshot.Platform } }
    }
    if ($SystemSnapshot.TotalMemoryBytes -le 0) {
        return [pscustomobject]@{ Code = 'memory.unknown'; Severity = 'Info'; Message = 'No hay capacidad de memoria medible.'; Evidence = @{ TotalMemoryBytes = $SystemSnapshot.TotalMemoryBytes } }
    }

    $freePercent = [math]::Round(($SystemSnapshot.FreeMemoryBytes / $SystemSnapshot.TotalMemoryBytes) * 100, 2)
    $severity = if ($freePercent -lt $Configuration.MemoryWarningPercent) { 'Warning' } else { 'Info' }
    [pscustomobject]@{ Code = 'memory.free-space'; Severity = $severity; Message = "La memoria física tiene $freePercent% disponible."; Evidence = @{ FreeMemoryBytes = $SystemSnapshot.FreeMemoryBytes; TotalMemoryBytes = $SystemSnapshot.TotalMemoryBytes; FreePercent = $freePercent } }
}

function Get-WorkstationAudit {
    [CmdletBinding()]
    param(
        [scriptblock]$SnapshotProvider = { Get-PlatformSnapshot },
        [scriptblock]$SystemProvider = { Get-WindowsSystemSnapshot },
        [hashtable]$Configuration = @{}
    )

    $resolvedConfiguration = Resolve-AuditConfiguration -Configuration $Configuration
    try {
        $snapshot = & $SnapshotProvider
        $systemSnapshot = & $SystemProvider
    }
    catch {
        throw [System.InvalidOperationException]::new('No se pudo recopilar el snapshot de la estación de trabajo.', $_.Exception)
    }
    if ($null -eq $snapshot -or $null -eq $systemSnapshot) {
        throw [System.InvalidOperationException]::new('Un proveedor de snapshot no devolvió datos.')
    }

    $findings = @(
        $snapshot.Drives | Get-StorageFinding -Configuration $resolvedConfiguration
        Get-MemoryFinding -SystemSnapshot $systemSnapshot -Configuration $resolvedConfiguration
    )
    [pscustomobject]@{
        SchemaVersion = 1
        Configuration = $resolvedConfiguration
        Snapshot = $snapshot
        System = $systemSnapshot
        Findings = $findings
        Summary = [pscustomobject]@{
            TotalFindings = $findings.Count
            Critical = @($findings | Where-Object Severity -eq 'Critical').Count
            Warning = @($findings | Where-Object Severity -eq 'Warning').Count
        }
    }
}

function Export-WorkstationAudit {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory, ValueFromPipeline)][psobject]$Audit,
        [Parameter(Mandatory)][ValidateNotNullOrEmpty()][string]$Path
    )
    process {
        $parent = Split-Path -Parent $Path
        if ($parent -and -not (Test-Path -LiteralPath $parent)) { New-Item -ItemType Directory -Path $parent -Force | Out-Null }
        $Audit | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath $Path -Encoding utf8NoBOM
        Get-Item -LiteralPath $Path
    }
}

Export-ModuleMember -Function Resolve-AuditConfiguration, Get-WindowsSystemSnapshot, Get-PlatformSnapshot, Get-StorageFinding, Get-MemoryFinding, Get-WorkstationAudit, Export-WorkstationAudit
