BeforeAll {
    Import-Module (Join-Path $PSScriptRoot '..' 'WorkstationAudit.psd1') -Force
}

Describe 'Resolve-AuditConfiguration' {
    It 'aplica defaults y permite personalizar umbrales' {
        $config = Resolve-AuditConfiguration -Configuration @{ StorageWarningPercent = 30; StorageCriticalPercent = 12; MemoryWarningPercent = 18 }
        $config.StorageWarningPercent | Should -Be 30
        $config.StorageCriticalPercent | Should -Be 12
        $config.MemoryWarningPercent | Should -Be 18
    }

    It 'rechaza un umbral crítico que no sea menor al warning' {
        { Resolve-AuditConfiguration -Configuration @{ StorageWarningPercent = 10; StorageCriticalPercent = 10 } } |
            Should -Throw '*Critical < Warning*'
    }
}

Describe 'Get-StorageFinding' {
    It 'clasifica como Info cuando hay al menos 20 por ciento libre' {
        $drive = [pscustomobject]@{ Name = 'T'; FreeBytes = 25; TotalBytes = 100 }
        $finding = $drive | Get-StorageFinding
        $finding.Severity | Should -Be 'Info'
        $finding.Evidence.FreePercent | Should -Be 25
    }

    It 'respeta configuración personalizada' {
        $drive = [pscustomobject]@{ Name = 'T'; FreeBytes = 25; TotalBytes = 100 }
        $config = Resolve-AuditConfiguration -Configuration @{ StorageWarningPercent = 30; StorageCriticalPercent = 10 }
        ($drive | Get-StorageFinding -Configuration $config).Severity | Should -Be 'Warning'
    }

    It 'clasifica como Critical por debajo de 10 por ciento' {
        $drive = [pscustomobject]@{ Name = 'T'; FreeBytes = 5; TotalBytes = 100 }
        ($drive | Get-StorageFinding).Severity | Should -Be 'Critical'
    }

    It 'no divide entre cero cuando la capacidad no es medible' {
        $drive = [pscustomobject]@{ Name = 'T'; FreeBytes = 0; TotalBytes = 0 }
        $finding = $drive | Get-StorageFinding
        $finding.Code | Should -Be 'storage.unknown'
        $finding.Severity | Should -Be 'Info'
    }
}

Describe 'Get-MemoryFinding' {
    It 'degrada a Info cuando la señal Windows no está soportada' {
        $finding = Get-MemoryFinding -SystemSnapshot ([pscustomobject]@{ Supported = $false; Platform = 'non-windows' })
        $finding.Code | Should -Be 'memory.not-supported'
        $finding.Severity | Should -Be 'Info'
    }

    It 'marca Warning cuando la memoria libre cruza el umbral' {
        $system = [pscustomobject]@{ Supported = $true; TotalMemoryBytes = 100; FreeMemoryBytes = 10 }
        (Get-MemoryFinding -SystemSnapshot $system).Severity | Should -Be 'Warning'
    }
}

Describe 'Get-WindowsSystemSnapshot' {
    It 'declara explícitamente no soportado fuera de Windows' -Skip:$IsWindows {
        $snapshot = Get-WindowsSystemSnapshot
        $snapshot.Supported | Should -BeFalse
        $snapshot.Platform | Should -Be 'non-windows'
    }
}

Describe 'Get-WorkstationAudit' {
    It 'compone snapshots, reglas y resumen sin depender del hardware real' {
        $provider = {
            [pscustomobject]@{
                ComputerName = 'fixture'
                OperatingSystem = 'fixture-os'
                PowerShellVersion = '7.fixture'
                CapturedAt = [DateTimeOffset]'2026-08-22T00:00:00Z'
                Drives = @(
                    [pscustomobject]@{ Name = 'A'; Root = '/a'; UsedBytes = 50; FreeBytes = 50; TotalBytes = 100 },
                    [pscustomobject]@{ Name = 'B'; Root = '/b'; UsedBytes = 95; FreeBytes = 5; TotalBytes = 100 }
                )
            }
        }
        $systemProvider = { [pscustomobject]@{ Supported = $true; Platform = 'windows'; TotalMemoryBytes = 100; FreeMemoryBytes = 50 } }

        $audit = Get-WorkstationAudit -SnapshotProvider $provider -SystemProvider $systemProvider
        $audit.SchemaVersion | Should -Be 1
        $audit.Findings.Count | Should -Be 3
        $audit.Summary.Critical | Should -Be 1
        $audit.Configuration.StorageWarningPercent | Should -Be 20
    }

    It 'convierte un fallo de recolección en un error explícito y contextual' {
        { Get-WorkstationAudit -SnapshotProvider { throw 'fixture failure' } } |
            Should -Throw '*No se pudo recopilar el snapshot*'
    }
}

Describe 'Export-WorkstationAudit' {
    It 'escribe JSON válido en una ruta nueva' {
        $path = Join-Path $TestDrive 'nested' 'audit.json'
        $audit = [pscustomobject]@{ SchemaVersion = 1; Findings = @(); Summary = @{ TotalFindings = 0 } }
        $file = $audit | Export-WorkstationAudit -Path $path
        $file.FullName | Should -Be (Get-Item $path).FullName
        (Get-Content $path -Raw | ConvertFrom-Json).SchemaVersion | Should -Be 1
    }
}
