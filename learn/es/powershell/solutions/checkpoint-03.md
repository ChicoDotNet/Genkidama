# Solución de referencia — Checkpoint 03

Una solución mínima mantiene el escenario como datos y prueba el delta semántico:

```powershell
$baseline = [pscustomobject]@{
    Snapshot = [pscustomobject]@{ ComputerName = 'checkpoint-03' }
    Findings = @([pscustomobject]@{ Code='storage.free-space'; Severity='Info'; Evidence=[pscustomobject]@{ Name='C' } })
}
$current = [pscustomobject]@{
    Snapshot = [pscustomobject]@{ ComputerName = 'checkpoint-03' }
    Findings = @(
        [pscustomobject]@{ Code='storage.free-space'; Severity='Warning'; Evidence=[pscustomobject]@{ Name='C' } },
        [pscustomobject]@{ Code='execution.elevated'; Severity='Warning'; Evidence=[pscustomobject]@{} }
    )
}
$delta = Compare-WorkstationAudit -Baseline $baseline -Current $current
$delta.Summary.Added | Should -Be 1
$delta.Summary.Changed | Should -Be 1
$delta.Summary.Resolved | Should -Be 0
```

La idea importante no es memorizar el hashtable: es separar identidad del finding, severidad y mensaje de presentación. Un cambio de redacción no debería parecer una regresión operacional.
