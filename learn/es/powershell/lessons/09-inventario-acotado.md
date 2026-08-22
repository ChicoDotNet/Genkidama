# Lección 09 — Inventario acotado de software y servicios

## Qué vas a conseguir
Añadirás a WorkstationAudit un inventario de software instalado y servicios de Windows sin convertir una auditoría en un volcado ilimitado de datos.

## El problema
`Get-Service` puede devolver cientos de elementos y el Registro contiene entradas duplicadas o incompletas. Guardar absolutamente todo aumenta ruido, tiempo y exposición de información sin mejorar automáticamente el diagnóstico.

## Concepto
PowerShell funciona mejor cuando conservas objetos hasta el borde. `Get-WindowsInventorySnapshot` consulta las claves de desinstalación de Windows y servicios, normaliza propiedades útiles y aplica `InventoryLimit`. Fuera de Windows devuelve `Supported = false`: una limitación declarada es mejor que datos inventados.

[DEMO] Ejecuta en Windows:

```powershell
Import-Module ./app/WorkstationAudit.psd1 -Force
Get-WindowsInventorySnapshot -Limit 10
```

Observa que `Software` y `Services` siguen siendo colecciones de objetos. No usamos `Format-Table` dentro del módulo: el formato pertenece a la interfaz, no al dato.

## Límites explícitos
El límite no pretende afirmar que los primeros 50 elementos sean los «más importantes». Es una frontera operativa y de privacidad para este curso. Una herramienta empresarial podría paginar, filtrar por editor, firmar inventarios o consultar una fuente administrada.

## Errores comunes
- Leer `Win32_Product` para inventariar software: puede ser lento y tener efectos de reparación MSI no deseados.
- Tratar una clave ausente como fallo global cuando otras entradas son válidas.
- Exportar cientos de propiedades que nadie usa.
- Afirmar soporte Linux para un inventario diseñado explícitamente alrededor del Registro de Windows.

## Tu turno
Configura `InventoryLimit = 15`, ejecuta el auditor y comprueba que el objeto `Inventory` conserva el límite resuelto.

## Cómo comprobar tu solución

```powershell
$audit = Get-WorkstationAudit -Configuration @{ InventoryLimit = 15 }
$audit.Inventory.Limit
```

## Siguiente paso
Continúa con [Lección 10 — Privilegios y seguridad de ejecución](10-privilegios-y-seguridad.md).

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.management/get-service
- https://learn.microsoft.com/powershell/module/microsoft.powershell.management/get-itemproperty
