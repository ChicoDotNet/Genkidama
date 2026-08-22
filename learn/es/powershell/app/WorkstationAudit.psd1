@{
    RootModule = 'WorkstationAudit.Root.psm1'
    ModuleVersion = '0.4.0'
    GUID = '7f476ac1-f32c-450a-a2f1-15107150d434'
    Author = 'Genkidama Learn'
    CompanyName = 'Genkidama'
    Copyright = '(c) Genkidama contributors. MIT.'
    Description = 'Núcleo del auditor de estaciones de trabajo de Genkidama Learn.'
    PowerShellVersion = '7.0'
    FunctionsToExport = @(
        'Resolve-AuditConfiguration', 'Get-WindowsSystemSnapshot', 'Get-WindowsInventorySnapshot',
        'Get-ExecutionContextSnapshot', 'Get-PlatformSnapshot', 'Get-StorageFinding', 'Get-MemoryFinding',
        'Get-PrivilegeFinding', 'Get-WorkstationAudit', 'Export-WorkstationAudit',
        'Export-WorkstationAuditText', 'Import-WorkstationAudit', 'Compare-WorkstationAudit',
        'Resolve-AuditTarget', 'Get-AuditFleetSummary', 'Measure-AuditOperation',
        'Test-WorkstationAuditContract', 'Get-WorkstationAuditExitCode'
    )
    CmdletsToExport = @()
    VariablesToExport = @()
    AliasesToExport = @()
}
