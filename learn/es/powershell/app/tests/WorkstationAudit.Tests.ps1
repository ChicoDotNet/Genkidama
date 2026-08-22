BeforeAll {
    Import-Module (Join-Path $PSScriptRoot '..' 'WorkstationAudit.psd1') -Force
}

Describe 'Resolve-AuditConfiguration' {
    It 'aplica defaults y permite personalizar umbrales e inventario' {
        $config = Resolve-AuditConfiguration -Configuration @{ StorageWarningPercent = 30; StorageCriticalPercent = 12; MemoryWarningPercent = 18; InventoryLimit = 25 }
        $config.StorageWarningPercent | Should -Be 30
        $config.StorageCriticalPercent | Should -Be 12
        $config.MemoryWarningPercent | Should -Be 18
        $config.InventoryLimit | Should -Be 25
    }
    It 'rechaza un umbral crítico que no sea menor al warning' {
        { Resolve-AuditConfiguration -Configuration @{ StorageWarningPercent = 10; StorageCriticalPercent = 10 } } | Should -Throw '*Critical < Warning*'
    }
    It 'rechaza inventarios sin límite razonable' {
        { Resolve-AuditConfiguration -Configuration @{ InventoryLimit = 0 } } | Should -Throw '*InventoryLimit*'
    }
}

Describe 'Get-StorageFinding' {
    It 'clasifica como Info cuando hay al menos 20 por ciento libre' {
        $finding = [pscustomobject]@{ Name = 'T'; FreeBytes = 25; TotalBytes = 100 } | Get-StorageFinding
        $finding.Severity | Should -Be 'Info'; $finding.Evidence.FreePercent | Should -Be 25
    }
    It 'respeta configuración personalizada' {
        $config = Resolve-AuditConfiguration -Configuration @{ StorageWarningPercent = 30; StorageCriticalPercent = 10 }
        ([pscustomobject]@{ Name = 'T'; FreeBytes = 25; TotalBytes = 100 } | Get-StorageFinding -Configuration $config).Severity | Should -Be 'Warning'
    }
    It 'clasifica como Critical por debajo de 10 por ciento' {
        ([pscustomobject]@{ Name = 'T'; FreeBytes = 5; TotalBytes = 100 } | Get-StorageFinding).Severity | Should -Be 'Critical'
    }
    It 'no divide entre cero cuando la capacidad no es medible' {
        $finding = [pscustomobject]@{ Name = 'T'; FreeBytes = 0; TotalBytes = 0 } | Get-StorageFinding
        $finding.Code | Should -Be 'storage.unknown'; $finding.Severity | Should -Be 'Info'
    }
}

Describe 'Get-MemoryFinding' {
    It 'degrada a Info cuando la señal Windows no está soportada' {
        (Get-MemoryFinding -SystemSnapshot ([pscustomobject]@{ Supported = $false; Platform = 'non-windows' })).Code | Should -Be 'memory.not-supported'
    }
    It 'marca Warning cuando la memoria libre cruza el umbral' {
        (Get-MemoryFinding -SystemSnapshot ([pscustomobject]@{ Supported = $true; TotalMemoryBytes = 100; FreeMemoryBytes = 10 })).Severity | Should -Be 'Warning'
    }
}

Describe 'Inventario y contexto de ejecución' {
    It 'declara el inventario Windows no soportado fuera de Windows' -Skip:$IsWindows {
        $inventory = Get-WindowsInventorySnapshot -Limit 7
        $inventory.Supported | Should -BeFalse; $inventory.Limit | Should -Be 7
    }
    It 'hace explícito cuando no puede evaluar elevación Windows' -Skip:$IsWindows {
        $context = Get-ExecutionContextSnapshot
        $context.Supported | Should -BeFalse; $context.IsElevated | Should -BeNullOrEmpty
    }
    It 'advierte cuando el proceso está elevado' {
        $finding = Get-PrivilegeFinding -ExecutionContext ([pscustomobject]@{ Supported = $true; UserName = 'fixture'; IsElevated = $true })
        $finding.Code | Should -Be 'execution.elevated'; $finding.Severity | Should -Be 'Warning'
    }
}

Describe 'Get-WorkstationAudit' {
    It 'compone snapshots, inventario, contexto y resumen sin depender del hardware real' {
        $provider = { [pscustomobject]@{ ComputerName = 'fixture'; OperatingSystem = 'fixture-os'; PowerShellVersion = '7.fixture'; CapturedAt = [DateTimeOffset]'2026-08-22T00:00:00Z'; Drives = @([pscustomobject]@{ Name = 'A'; Root = '/a'; UsedBytes = 50; FreeBytes = 50; TotalBytes = 100 }, [pscustomobject]@{ Name = 'B'; Root = '/b'; UsedBytes = 95; FreeBytes = 5; TotalBytes = 100 }) } }
        $systemProvider = { [pscustomobject]@{ Supported = $true; Platform = 'windows'; TotalMemoryBytes = 100; FreeMemoryBytes = 50 } }
        $inventoryProvider = { param($Limit) [pscustomobject]@{ Supported = $true; Platform = 'windows'; Limit = $Limit; Software = @([pscustomobject]@{ DisplayName = 'FixtureApp' }); Services = @() } }
        $executionProvider = { [pscustomobject]@{ Supported = $true; Platform = 'windows'; UserName = 'fixture'; IsElevated = $false; ExecutionPolicy = @() } }
        $audit = Get-WorkstationAudit -SnapshotProvider $provider -SystemProvider $systemProvider -InventoryProvider $inventoryProvider -ExecutionContextProvider $executionProvider
        $audit.SchemaVersion | Should -Be 2
        $audit.Findings.Count | Should -Be 4
        $audit.Summary.Critical | Should -Be 1
        $audit.Inventory.Software[0].DisplayName | Should -Be 'FixtureApp'
        $audit.Configuration.InventoryLimit | Should -Be 50
    }
    It 'convierte un fallo de recolección en un error explícito y contextual' {
        { Get-WorkstationAudit -SnapshotProvider { throw 'fixture failure' } } | Should -Throw '*No se pudo recopilar el snapshot*'
    }
}

Describe 'Reportes y comparación' {
    It 'escribe e importa JSON válido' {
        $path = Join-Path $TestDrive 'nested' 'audit.json'
        $audit = [pscustomobject]@{ SchemaVersion = 2; Snapshot = [pscustomobject]@{ ComputerName = 'fixture' }; Findings = @(); Summary = @{ TotalFindings = 0 } }
        $audit | Export-WorkstationAudit -Path $path | Out-Null
        (Import-WorkstationAudit -Path $path).SchemaVersion | Should -Be 2
    }
    It 'genera un reporte humano legible' {
        $path = Join-Path $TestDrive 'audit.txt'
        $audit = [pscustomobject]@{ Snapshot = [pscustomobject]@{ ComputerName = 'fixture'; CapturedAt = 'now' }; Findings = @([pscustomobject]@{ Severity = 'Warning'; Code = 'fixture'; Message = 'mensaje' }); Summary = [pscustomobject]@{ TotalFindings = 1; Critical = 0; Warning = 1 } }
        $audit | Export-WorkstationAuditText -Path $path | Out-Null
        Get-Content $path -Raw | Should -Match '\[Warning\] fixture - mensaje'
    }
    It 'detecta hallazgos agregados, resueltos y cambios de severidad' {
        $baseline = [pscustomobject]@{ Snapshot = [pscustomobject]@{ ComputerName = 'fixture' }; Findings = @([pscustomobject]@{ Code = 'storage.free-space'; Severity = 'Info'; Evidence = [pscustomobject]@{ Name = 'C' } }, [pscustomobject]@{ Code = 'old'; Severity = 'Warning'; Evidence = [pscustomobject]@{} }) }
        $current = [pscustomobject]@{ Snapshot = [pscustomobject]@{ ComputerName = 'fixture' }; Findings = @([pscustomobject]@{ Code = 'storage.free-space'; Severity = 'Warning'; Evidence = [pscustomobject]@{ Name = 'C' } }, [pscustomobject]@{ Code = 'new'; Severity = 'Info'; Evidence = [pscustomobject]@{} }) }
        $comparison = Compare-WorkstationAudit -Baseline $baseline -Current $current
        $comparison.Summary.Added | Should -Be 1
        $comparison.Summary.Resolved | Should -Be 1
        $comparison.Summary.Changed | Should -Be 1
    }
    It 'rechaza comparar equipos distintos' {
        $a = [pscustomobject]@{ Snapshot = [pscustomobject]@{ ComputerName = 'A' }; Findings = @() }
        $b = [pscustomobject]@{ Snapshot = [pscustomobject]@{ ComputerName = 'B' }; Findings = @() }
        { Compare-WorkstationAudit -Baseline $a -Current $b } | Should -Throw '*equipos diferentes*'
    }
}

Describe 'Alcance, fan-out, profiling y contrato' {
    It 'trata localhost como destino local sin cambios de configuración' {
        $target = Resolve-AuditTarget -ComputerName localhost
        $target.Mode | Should -Be 'Local'
        $target.RequiresRemoting | Should -BeFalse
        $target.ChangesSystemConfiguration | Should -BeFalse
    }
    It 'requiere opt-in explícito para un destino remoto' {
        { Resolve-AuditTarget -ComputerName 'server-01' } | Should -Throw '*-AllowRemote*'
        (Resolve-AuditTarget -ComputerName 'server-01' -AllowRemote).Mode | Should -Be 'Remote'
    }
    It 'resume múltiples reportes igual en secuencial y paralelo' {
        $paths = @()
        foreach ($item in @(@{Name='B';Severity='Warning'}, @{Name='A';Severity='Critical'}, @{Name='C';Severity='Info'})) {
            $path = Join-Path $TestDrive "$($item.Name).json"
            [pscustomobject]@{ SchemaVersion = 2; Snapshot = [pscustomobject]@{ ComputerName = $item.Name }; Findings = @([pscustomobject]@{ Code='fixture'; Severity=$item.Severity }) } | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $path
            $paths += $path
        }
        $serial = Get-AuditFleetSummary -Path $paths -ThrottleLimit 1
        $parallel = Get-AuditFleetSummary -Path $paths -ThrottleLimit 2
        $serial.ReportCount | Should -Be 3
        $parallel.TotalFindings | Should -Be $serial.TotalFindings
        $parallel.Critical | Should -Be 1
        $parallel.Warning | Should -Be 1
        @($parallel.Reports.ComputerName) | Should -Be @('A','B','C')
    }
    It 'mide sin imponer un umbral de rendimiento' {
        $measurement = Measure-AuditOperation -Name fixture -Operation { 42 }
        $measurement.Result | Should -Be 42
        $measurement.ElapsedMilliseconds | Should -BeGreaterOrEqual 0
    }
    It 'valida contrato y semántica de severidad' {
        $valid = [pscustomobject]@{ SchemaVersion=2; Snapshot=[pscustomobject]@{ComputerName='fixture'}; Findings=@([pscustomobject]@{Code='x';Severity='Info'}) }
        ($valid | Test-WorkstationAuditContract).IsValid | Should -BeTrue
        $invalid = [pscustomobject]@{ SchemaVersion=2; Snapshot=[pscustomobject]@{ComputerName='fixture'}; Findings=@([pscustomobject]@{Code='';Severity='Boom'}) }
        ($invalid | Test-WorkstationAuditContract).IsValid | Should -BeFalse
    }
    It 'traduce severidad a códigos de salida automatizables' {
        Get-WorkstationAuditExitCode -Audit ([pscustomobject]@{Summary=[pscustomobject]@{Critical=1;Warning=0}}) | Should -Be 2
        Get-WorkstationAuditExitCode -Audit ([pscustomobject]@{Summary=[pscustomobject]@{Critical=0;Warning=1}}) | Should -Be 1
        Get-WorkstationAuditExitCode -Audit ([pscustomobject]@{Summary=[pscustomobject]@{Critical=0;Warning=0}}) | Should -Be 0
    }
}
