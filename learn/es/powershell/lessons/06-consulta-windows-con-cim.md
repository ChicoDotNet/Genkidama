# Lección 06 — Consulta Windows con CIM y una frontera explícita

## Qué vas a conseguir
Añadirás señales reales de Windows sin fingir que el mismo API existe en todas las plataformas.

## El problema
PowerShell es multiplataforma; `Win32_OperatingSystem` no lo es. Un script profesional distingue capacidad no soportada de una operación soportada que falló.

## Concepto
`Get-WindowsSystemSnapshot` encierra las consultas CIM. Fuera de Windows devuelve un objeto `Supported = $false`. En Windows, una excepción CIM se conserva como fallo contextual.

[DEMO]
```powershell
Get-WindowsSystemSnapshot | Format-List
```

En Windows verás versión, arranque y memoria. En Linux verás una declaración explícita de no soporte.

## Código real
Ver implementación: `../app/WorkstationAudit.psm1`.

## Errores comunes
- Llamar `Get-CimInstance` por todo el módulo.
- Capturar todas las excepciones y devolver `$null`.
- Presentar “no soportado” como “equipo saludable”.

## Tu turno
Inspecciona el objeto y explica qué campos son portables y cuáles dependen de Win32.

## Siguiente paso
[Lección 07 — Reglas reutilizables de memoria](07-reglas-reutilizables-de-memoria.md)

## Referencias
- https://learn.microsoft.com/powershell/module/cimcmdlets/get-ciminstance
- https://learn.microsoft.com/windows/win32/cimwin32prov/win32-operatingsystem
