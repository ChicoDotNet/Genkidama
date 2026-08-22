@{
    RootModule = 'WorkstationAudit.psm1'
    ModuleVersion = '0.1.0'
    GUID = '7f476ac1-f32c-450a-a2f1-15107150d434'
    Author = 'Genkidama Learn'
    CompanyName = 'Genkidama'
    Copyright = '(c) Genkidama contributors. MIT.'
    Description = 'Núcleo del auditor de estaciones de trabajo de Genkidama Learn.'
    PowerShellVersion = '7.0'
    FunctionsToExport = @(
        'Get-PlatformSnapshot',
        'Get-StorageFinding',
        'Get-WorkstationAudit',
        'Export-WorkstationAudit'
    )
    CmdletsToExport = @()
    VariablesToExport = @()
    AliasesToExport = @()
}
