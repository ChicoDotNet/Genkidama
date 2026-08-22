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

El comparador separa:
- `Added`: aparece ahora y no antes.
- `Resolved`: existía antes y ya no aparece.
- `Changed`: conserva identidad pero cambió severidad.

También rechaza comparar nombres de equipo distintos. Una comparación técnicamente posible pero semánticamente falsa es peor que un error explícito.

## Persistencia no es una base de datos
JSON es suficiente para aprender contrato, serialización y evolución de esquema. No añadimos SQLite ni un servicio sólo por parecer más «enterprise». Si el historial crece, esa necesidad será observable y podremos cambiar el almacenamiento detrás de una frontera clara.

## Errores comunes
- Comparar mensajes localizados en vez de identidades estables.
- Asumir que ausencia significa resolución cuando la recolección falló.
- Comparar equipos diferentes.
- Borrar el baseline después de cada ejecución sin política de retención.

## Tu turno
Crea dos fixtures con una unidad `C`, cambia su severidad y añade otro finding. Verifica `Added = 1` y `Changed = 1`.

## Cómo comprobar tu solución
Ejecuta Pester; el curso contiene un escenario determinista de agregado, resuelto y cambio de severidad.

## Siguiente paso
Completa [Checkpoint 03](../exercises/checkpoint-03.md) antes de continuar con remoting y alcance seguro.

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.utility/convertfrom-json
