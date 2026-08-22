BeforeAll {
    Import-Module (Join-Path $PSScriptRoot '..' 'WorkstationAudit.psd1') -Force
}

Describe 'Get-StorageFinding' {
    It 'clasifica como Info cuando hay al menos 20 por ciento libre' {
        $drive = [pscustomobject]@{ Name = 'T'; FreeBytes = 25; TotalBytes = 100 }
        $finding = $drive | Get-StorageFinding
        $finding.Severity | Should -Be 'Info'
        $finding.Evidence.FreePercent | Should -Be 25
    }

    It 'clasifica como Warning por debajo de 20 por ciento' {
        $drive = [pscustomobject]@{ Name = 'T'; FreeBytes = 15; TotalBytes = 100 }
        ($drive | Get-StorageFinding).Severity | Should -Be 'Warning'
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

Describe 'Get-WorkstationAudit' {
    It 'compone snapshot, findings y resumen sin depender del hardware real' {
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

        $audit = Get-WorkstationAudit -SnapshotProvider $provider
        $audit.SchemaVersion | Should -Be 1
        $audit.Findings.Count | Should -Be 2
        $audit.Summary.Critical | Should -Be 1
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
