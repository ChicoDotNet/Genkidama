Set-StrictMode -Version Latest

function Resolve-AuditConfiguration {
    [CmdletBinding()]
    param([hashtable]$Configuration = @{})

    $warning = if ($Configuration.ContainsKey('StorageWarningPercent')) { [double]$Configuration.StorageWarningPercent } else { 20.0 }
    $critical = if ($Configuration.ContainsKey('StorageCriticalPercent')) { [double]$Configuration.StorageCriticalPercent } else { 10.0 }
    $memoryWarning = if ($Configuration.ContainsKey('MemoryWarningPercent')) { [double]$Configuration.MemoryWarningPercent } else { 15.0 }
    $inventoryLimit = if ($Configuration.ContainsKey('InventoryLimit')) { [int]$Configuration.InventoryLimit } else { 50 }

    if ($critical -lt 0 -or $warning -le 0 -or $warning -gt 100 -or $critical -ge $warning) {
        throw [System.ArgumentException]::new('Los umbrales de almacenamiento requieren 0 <= Critical < Warning <= 100.')
    }
    if ($memoryWarning -le 0 -or $memoryWarning -gt 100) {
        throw [System.ArgumentException]::new('MemoryWarningPercent debe estar entre 0 y 100.')
    }
    if ($inventoryLimit -lt 1 -or $inventoryLimit -gt 500) {
        throw [System.ArgumentException]::new('InventoryLimit debe estar entre 1 y 500.')
    }

    [pscustomobject]@{
        StorageWarningPercent  = $warning
        StorageCriticalPercent = $critical
        MemoryWarningPercent   = $memoryWarning
        InventoryLimit         = $inventoryLimit
    }
}

function Get-WindowsSystemSnapshot {
    [CmdletBinding()]
    param()

    if (-not $IsWindows) {
        return [pscustomobject]@{ Supported = $false; Platform = 'non-windows'; Reason = 'Las consultas CIM de Win32 sólo se ejecutan en Windows.' }
    }

    try {
        $os = Get-CimInstance -ClassName Win32_OperatingSystem -ErrorAction Stop
        $computer = Get-CimInstance -ClassName Win32_ComputerSystem -ErrorAction Stop
    }
    catch {
        throw [System.InvalidOperationException]::new('No se pudieron consultar las señales CIM de Windows.', $_.Exception)
    }

    [pscustomobject]@{
        Supported = $true
        Platform = 'windows'
        Caption = $os.Caption
        Version = $os.Version
        LastBootUpTime = $os.LastBootUpTime
        TotalMemoryBytes = [long]$computer.TotalPhysicalMemory
        FreeMemoryBytes = [long]$os.FreePhysicalMemory * 1KB
        Manufacturer = $computer.Manufacturer
        Model = $computer.Model
    }
}

function Get-WindowsInventorySnapshot {
    [CmdletBinding()]
    param([ValidateRange(1, 500)][int]$Limit = 50)

    if (-not $IsWindows) {
        return [pscustomobject]@{ Supported = $false; Platform = 'non-windows'; Limit = $Limit; Software = @(); Services = @(); Reason = 'El inventario de Registro y servicios de esta lección sólo se ejecuta en Windows.' }
    }

    try {
        $registryPaths = @(
            'HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*',
            'HKLM:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\*'
        )
        $software = @(
            Get-ItemProperty -Path $registryPaths -ErrorAction SilentlyContinue |
                Where-Object { -not [string]::IsNullOrWhiteSpace($_.DisplayName) } |
                Sort-Object DisplayName, DisplayVersion -Unique |
                Select-Object -First $Limit DisplayName, DisplayVersion, Publisher
        )
        $services = @(
            Get-Service -ErrorAction Stop |
                Sort-Object Name |
                Select-Object -First $Limit Name, DisplayName, Status, StartType
        )
    }
    catch {
        throw [System.InvalidOperationException]::new('No se pudo recopilar el inventario acotado de Windows.', $_.Exception)
    }

    [pscustomobject]@{ Supported = $true; Platform = 'windows'; Limit = $Limit; Software = $software; Services = $services }
}

function Get-ExecutionContextSnapshot {
    [CmdletBinding()]
    param()

    if (-not $IsWindows) {
        return [pscustomobject]@{
            Supported = $false
            Platform = 'non-windows'
            UserName = [Environment]::UserName
            IsElevated = $null
            ExecutionPolicy = @()
            Reason = 'La comprobación de token administrativo de esta lección es específica de Windows.'
        }
    }

    try {
        $identity = [Security.Principal.WindowsIdentity]::GetCurrent()
        $principal = [Security.Principal.WindowsPrincipal]::new($identity)
        $isElevated = $principal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
        $policy = @(Get-ExecutionPolicy -List | ForEach-Object { [pscustomobject]@{ Scope = $_.Scope.ToString(); ExecutionPolicy = $_.ExecutionPolicy.ToString() } })
    }
    catch {
        throw [System.InvalidOperationException]::new('No se pudo determinar el contexto de ejecución de Windows.', $_.Exception)
    }

    [pscustomobject]@{ Supported = $true; Platform = 'windows'; UserName = $identity.Name; IsElevated = $isElevated; ExecutionPolicy = $policy }
}

function Get-PlatformSnapshot {
    [CmdletBinding()]
    param()

    $drives = @(Get-PSDrive -PSProvider FileSystem | ForEach-Object {
        $used = if ($null -eq $_.Used) { 0L } else { [long]$_.Used }
        $free = if ($null -eq $_.Free) { 0L } else { [long]$_.Free }
        [pscustomobject]@{ Name = $_.Name; Root = $_.Root; UsedBytes = $used; FreeBytes = $free; TotalBytes = $used + $free }
    })

    [pscustomobject]@{
        ComputerName = [Environment]::MachineName
        OperatingSystem = [System.Runtime.InteropServices.RuntimeInformation]::OSDescription
        PowerShellVersion = $PSVersionTable.PSVersion.ToString()
        CapturedAt = [DateTimeOffset]::UtcNow
        Drives = $drives
    }
}

function Get-StorageFinding {
    [CmdletBinding()]
    param([Parameter(Mandatory, ValueFromPipeline)][psobject]$Drive, [psobject]$Configuration = (Resolve-AuditConfiguration))
    process {
        if ($Drive.TotalBytes -le 0) {
            return [pscustomobject]@{ Code = 'storage.unknown'; Severity = 'Info'; Message = "No hay capacidad medible para $($Drive.Name)."; Evidence = @{ Name = $Drive.Name; TotalBytes = $Drive.TotalBytes } }
        }
        $freePercent = [math]::Round(($Drive.FreeBytes / $Drive.TotalBytes) * 100, 2)
        $severity = if ($freePercent -lt $Configuration.StorageCriticalPercent) { 'Critical' } elseif ($freePercent -lt $Configuration.StorageWarningPercent) { 'Warning' } else { 'Info' }
        [pscustomobject]@{ Code = 'storage.free-space'; Severity = $severity; Message = "La unidad $($Drive.Name) tiene $freePercent% libre."; Evidence = @{ Name = $Drive.Name; FreeBytes = $Drive.FreeBytes; TotalBytes = $Drive.TotalBytes; FreePercent = $freePercent } }
    }
}

function Get-MemoryFinding {
    [CmdletBinding()]
    param([Parameter(Mandatory)][psobject]$SystemSnapshot, [psobject]$Configuration = (Resolve-AuditConfiguration))

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

function Get-PrivilegeFinding {
    [CmdletBinding()]
    param([Parameter(Mandatory)][psobject]$ExecutionContext)

    if (-not $ExecutionContext.Supported) {
        return [pscustomobject]@{ Code = 'execution.context-limited'; Severity = 'Info'; Message = 'El auditor no evaluó elevación administrativa en esta plataforma.'; Evidence = @{ Platform = $ExecutionContext.Platform } }
    }
    if ($ExecutionContext.IsElevated) {
        return [pscustomobject]@{ Code = 'execution.elevated'; Severity = 'Warning'; Message = 'El auditor se está ejecutando con privilegios administrativos; evita mezclar diagnóstico con remediación.'; Evidence = @{ UserName = $ExecutionContext.UserName; IsElevated = $true } }
    }
    [pscustomobject]@{ Code = 'execution.standard-user'; Severity = 'Info'; Message = 'El auditor se ejecuta sin elevación administrativa.'; Evidence = @{ UserName = $ExecutionContext.UserName; IsElevated = $false } }
}

function Get-WorkstationAudit {
    [CmdletBinding()]
    param(
        [scriptblock]$SnapshotProvider = { Get-PlatformSnapshot },
        [scriptblock]$SystemProvider = { Get-WindowsSystemSnapshot },
        [scriptblock]$InventoryProvider = { param($Limit) Get-WindowsInventorySnapshot -Limit $Limit },
        [scriptblock]$ExecutionContextProvider = { Get-ExecutionContextSnapshot },
        [hashtable]$Configuration = @{}
    )

    $resolvedConfiguration = Resolve-AuditConfiguration -Configuration $Configuration
    try {
        $snapshot = & $SnapshotProvider
        $systemSnapshot = & $SystemProvider
        $inventory = & $InventoryProvider $resolvedConfiguration.InventoryLimit
        $executionContext = & $ExecutionContextProvider
    }
    catch {
        throw [System.InvalidOperationException]::new('No se pudo recopilar el snapshot de la estación de trabajo.', $_.Exception)
    }
    if ($null -eq $snapshot -or $null -eq $systemSnapshot -or $null -eq $inventory -or $null -eq $executionContext) {
        throw [System.InvalidOperationException]::new('Un proveedor de snapshot no devolvió datos.')
    }

    $findings = @(
        $snapshot.Drives | Get-StorageFinding -Configuration $resolvedConfiguration
        Get-MemoryFinding -SystemSnapshot $systemSnapshot -Configuration $resolvedConfiguration
        Get-PrivilegeFinding -ExecutionContext $executionContext
    )
    [pscustomobject]@{
        SchemaVersion = 2
        Configuration = $resolvedConfiguration
        Snapshot = $snapshot
        System = $systemSnapshot
        Inventory = $inventory
        ExecutionContext = $executionContext
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
    param([Parameter(Mandatory, ValueFromPipeline)][psobject]$Audit, [Parameter(Mandatory)][ValidateNotNullOrEmpty()][string]$Path)
    process {
        $parent = Split-Path -Parent $Path
        if ($parent -and -not (Test-Path -LiteralPath $parent)) { New-Item -ItemType Directory -Path $parent -Force | Out-Null }
        $Audit | ConvertTo-Json -Depth 10 | Set-Content -LiteralPath $Path -Encoding utf8NoBOM
        Get-Item -LiteralPath $Path
    }
}

function Export-WorkstationAuditText {
    [CmdletBinding()]
    param([Parameter(Mandatory, ValueFromPipeline)][psobject]$Audit, [Parameter(Mandatory)][ValidateNotNullOrEmpty()][string]$Path)
    process {
        $parent = Split-Path -Parent $Path
        if ($parent -and -not (Test-Path -LiteralPath $parent)) { New-Item -ItemType Directory -Path $parent -Force | Out-Null }
        $lines = @(
            'WorkstationAudit',
            "Equipo: $($Audit.Snapshot.ComputerName)",
            "Capturado: $($Audit.Snapshot.CapturedAt)",
            "Hallazgos: $($Audit.Summary.TotalFindings) | Critical: $($Audit.Summary.Critical) | Warning: $($Audit.Summary.Warning)",
            ''
        )
        $lines += @($Audit.Findings | ForEach-Object { "[$($_.Severity)] $($_.Code) - $($_.Message)" })
        $lines | Set-Content -LiteralPath $Path -Encoding utf8NoBOM
        Get-Item -LiteralPath $Path
    }
}

function Import-WorkstationAudit {
    [CmdletBinding()]
    param([Parameter(Mandatory)][ValidateNotNullOrEmpty()][string]$Path)

    if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { throw [System.IO.FileNotFoundException]::new("No existe el reporte: $Path") }
    try { $audit = Get-Content -LiteralPath $Path -Raw -ErrorAction Stop | ConvertFrom-Json -ErrorAction Stop }
    catch { throw [System.InvalidDataException]::new('El reporte no contiene JSON válido de WorkstationAudit.', $_.Exception) }
    if ($null -eq $audit.SchemaVersion -or $null -eq $audit.Findings -or $null -eq $audit.Snapshot) { throw [System.InvalidDataException]::new('El reporte no contiene el contrato mínimo de WorkstationAudit.') }
    $audit
}

function Get-FindingIdentity {
    param([Parameter(Mandatory)][psobject]$Finding)
    $name = $null
    if ($null -ne $Finding.Evidence -and $null -ne $Finding.Evidence.PSObject.Properties['Name']) { $name = $Finding.Evidence.Name }
    if ($name) { return "$($Finding.Code)|$name" }
    $Finding.Code
}

function Compare-WorkstationAudit {
    [CmdletBinding()]
    param([Parameter(Mandatory)][psobject]$Baseline, [Parameter(Mandatory)][psobject]$Current)

    if ($Baseline.Snapshot.ComputerName -and $Current.Snapshot.ComputerName -and $Baseline.Snapshot.ComputerName -ne $Current.Snapshot.ComputerName) {
        throw [System.ArgumentException]::new('No se comparan auditorías de equipos diferentes.')
    }

    $before = @{}; foreach ($finding in @($Baseline.Findings)) { $before[(Get-FindingIdentity $finding)] = $finding }
    $after = @{}; foreach ($finding in @($Current.Findings)) { $after[(Get-FindingIdentity $finding)] = $finding }
    $added = @(); $resolved = @(); $changed = @()
    foreach ($key in $after.Keys) {
        if (-not $before.ContainsKey($key)) { $added += $after[$key] }
        elseif ($before[$key].Severity -ne $after[$key].Severity) { $changed += [pscustomobject]@{ Identity = $key; Before = $before[$key].Severity; After = $after[$key].Severity } }
    }
    foreach ($key in $before.Keys) { if (-not $after.ContainsKey($key)) { $resolved += $before[$key] } }
    [pscustomobject]@{ ComputerName = $Current.Snapshot.ComputerName; Added = $added; Resolved = $resolved; Changed = $changed; Summary = [pscustomobject]@{ Added = $added.Count; Resolved = $resolved.Count; Changed = $changed.Count } }
}

Export-ModuleMember -Function Resolve-AuditConfiguration, Get-WindowsSystemSnapshot, Get-WindowsInventorySnapshot, Get-ExecutionContextSnapshot, Get-PlatformSnapshot, Get-StorageFinding, Get-MemoryFinding, Get-PrivilegeFinding, Get-WorkstationAudit, Export-WorkstationAudit, Export-WorkstationAuditText, Import-WorkstationAudit, Compare-WorkstationAudit
