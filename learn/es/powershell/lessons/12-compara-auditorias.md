# Lección 12 — Compara auditorías persistidas

## Qué vas a conseguir
Cargarás una auditoría previa y detectarás hallazgos nuevos, resueltos o cuyo nivel de severidad cambió.

## El problema
Una fotografía aislada responde «¿cómo está el equipo?». Operaciones también necesita «¿qué cambió desde la última vez?». Comparar texto con `diff` confunde formato con significado.

## Concepto
`Import-WorkstationAudit` valida el contrato mínimo del JSON antes de usarlo. `Compare-WorkstationAudit` crea una identidad estable para cada finding; para almacenamiento combina código + nombre de unidad, y para findings únicos usa el código.

```powershell
$baseline = Import-WorkstationAudit -Path ./audit-anterior.json
$current = Get-WorkstationAudit
$delta = Compare-WorkstationAudit -Baseline $baseline -Current $current
$delta.Summary
```

El comparador separa `Added`, `Resolved` y `Changed`, y rechaza comparar equipos diferentes.

## Tu turno
Crea dos fixtures con una unidad `C`, cambia su severidad y añade otro finding. Verifica `Added = 1` y `Changed = 1`.

## Cómo comprobar tu solución
Ejecuta Pester; el curso contiene un escenario determinista de agregado, resuelto y cambio de severidad.

## Siguiente paso
Completa [Checkpoint 03](../exercises/checkpoint-03.md) y continúa con [Lección 13 — Remoting con alcance explícito](13-remoting-con-alcance-explicito.md).

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.utility/convertfrom-json
