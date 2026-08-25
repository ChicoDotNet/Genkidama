# Lección 05 — Configura reglas sin editar código

## Qué vas a conseguir
Harás configurables los umbrales del auditor sin esconder reglas dentro de números mágicos.

## El problema
20% y 10% pueden ser razonables para una estación, pero una organización puede necesitar otros límites. Editar funciones para cada equipo mezcla política con implementación.

## Concepto
Una configuración es entrada. Primero se valida y después se transforma en un objeto normalizado que las reglas reciben explícitamente.

[DEMO]
```powershell
Resolve-AuditConfiguration
Resolve-AuditConfiguration -Configuration @{
    StorageWarningPercent = 30
    StorageCriticalPercent = 12
}
```

Una combinación inválida falla temprano con un mensaje claro.

## Código real
Ver implementación: `../app/WorkstationAudit.psm1`.

## Tu turno
Cambia `MemoryWarningPercent` a 18 y comprueba el objeto. Luego intenta un crítico igual al warning.

## Cómo comprobar tu solución
Ejecuta Pester y confirma que la validación rechaza `Critical >= Warning`.

## Resumen
La configuración mueve decisiones operativas fuera de la lógica sin sacrificar validación.

## Siguiente paso
[Lección 06 — Consulta Windows con CIM](06-consulta-windows-con-cim.md)

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_hash_tables
