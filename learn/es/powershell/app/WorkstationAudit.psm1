Set-StrictMode -Version Latest

function Get-PlatformSnapshot {
    [CmdletBinding()]
    param()

    $drives = @(
        Get-PSDrive -PSProvider FileSystem | ForEach-Object {
            $used = if ($null -eq $_.Used) { 0L } else { [long]$_.Used }
            $free = if ($null -eq $_.Free) { 0L } else { [long]$_.Free }
            $total = $used + $free

            [pscustomobject]@{
                Name       = $_.Name
                Root       = $_.Root
                UsedBytes  = $used
                FreeBytes  = $free
                TotalBytes = $total
            }
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
        [Parameter(Mandatory, ValueFromPipeline)]
        [psobject]$Drive
    )

    process {
        if ($Drive.TotalBytes -le 0) {
            return [pscustomobject]@{
                Code     = 'storage.unknown'
                Severity = 'Info'
                Message  = "No hay capacidad medible para $($Drive.Name)."
                Evidence = @{ Name = $Drive.Name; TotalBytes = $Drive.TotalBytes }
            }
        }

        $freePercent = [math]::Round(($Drive.FreeBytes / $Drive.TotalBytes) * 100, 2)
        $severity = if ($freePercent -lt 10) {
            'Critical'
        }
        elseif ($freePercent -lt 20) {
            'Warning'
        }
        else {
            'Info'
        }

        [pscustomobject]@{
            Code     = 'storage.free-space'
            Severity = $severity
            Message  = "La unidad $($Drive.Name) tiene $freePercent% libre."
            Evidence = @{
                Name        = $Drive.Name
                FreeBytes   = $Drive.FreeBytes
                TotalBytes  = $Drive.TotalBytes
                FreePercent = $freePercent
            }
        }
    }
}

function Get-WorkstationAudit {
    [CmdletBinding()]
    param(
        [scriptblock]$SnapshotProvider = { Get-PlatformSnapshot }
    )

    try {
        $snapshot = & $SnapshotProvider
    }
    catch {
        throw [System.InvalidOperationException]::new(
            'No se pudo recopilar el snapshot de la estación de trabajo.',
            $_.Exception
        )
    }

    if ($null -eq $snapshot) {
        throw [System.InvalidOperationException]::new('El proveedor de snapshot no devolvió datos.')
    }

    $findings = @($snapshot.Drives | Get-StorageFinding)
    $criticalCount = @($findings | Where-Object Severity -eq 'Critical').Count
    $warningCount = @($findings | Where-Object Severity -eq 'Warning').Count

    [pscustomobject]@{
        SchemaVersion = 1
        Snapshot      = $snapshot
        Findings      = $findings
        Summary       = [pscustomobject]@{
            TotalFindings = $findings.Count
            Critical      = $criticalCount
            Warning       = $warningCount
        }
    }
}

function Export-WorkstationAudit {
    [CmdletBinding()]
    param(
        [Parameter(Mandatory, ValueFromPipeline)]
        [psobject]$Audit,

        [Parameter(Mandatory)]
        [ValidateNotNullOrEmpty()]
        [string]$Path
    )

    process {
        $parent = Split-Path -Parent $Path
        if ($parent -and -not (Test-Path -LiteralPath $parent)) {
            New-Item -ItemType Directory -Path $parent -Force | Out-Null
        }

        $Audit | ConvertTo-Json -Depth 8 | Set-Content -LiteralPath $Path -Encoding utf8NoBOM
        Get-Item -LiteralPath $Path
    }
}

Export-ModuleMember -Function Get-PlatformSnapshot, Get-StorageFinding, Get-WorkstationAudit, Export-WorkstationAudit
