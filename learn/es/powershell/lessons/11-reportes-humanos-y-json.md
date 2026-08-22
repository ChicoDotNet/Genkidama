# Lección 11 — Reportes para humanos y máquinas

## Qué vas a conseguir
Generarás dos representaciones del mismo diagnóstico: JSON para automatización y texto para lectura rápida.

## El problema
`Format-Table` es excelente para consola, pero no es un contrato de datos. Un reporte que sólo se ve bonito no es fácil de comparar; un JSON completo tampoco es necesariamente agradable para una persona de soporte.

## Concepto
El objeto de auditoría es la fuente de verdad. `Export-WorkstationAudit` lo serializa a JSON y `Export-WorkstationAuditText` deriva un resumen humano. Ninguna función vuelve a consultar el sistema.

```powershell
$audit = Get-WorkstationAudit
$audit | Export-WorkstationAudit -Path ./audit.json
$audit | Export-WorkstationAuditText -Path ./audit.txt
```

[EN PANTALLA] Abre ambos archivos. El JSON conserva evidencia estructurada; el texto prioriza equipo, momento, resumen y hallazgos.

## Separar dato de presentación
Mantener `Findings` como objetos permite enviar mañana el mismo contrato a CSV, HTML o una API sin reescribir las reglas. Por eso el módulo no devuelve texto coloreado como resultado primario.

## Manejo de rutas
Los exportadores crean el directorio padre cuando hace falta y escriben UTF-8 sin BOM. Un fallo de disco o permisos debe propagarse: un reporte que no se escribió no cuenta como éxito.

## Tu turno
Genera JSON y texto en una carpeta nueva y comprueba que ambos nacen del mismo `$audit`.

## Reto adicional
Añade una representación Markdown en tu rama de práctica sin modificar las funciones de recolección.

## Siguiente paso
Continúa con [Lección 12 — Compara auditorías persistidas](12-compara-auditorias.md).

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.utility/convertto-json
- https://learn.microsoft.com/powershell/module/microsoft.powershell.management/set-content
