# Lección 14 — Fan-out con concurrencia acotada

## Qué vas a conseguir
Procesarás varios reportes independientes en paralelo sin asumir que más hilos siempre son mejores.

## El problema
Leer decenas de reportes es una operación naturalmente paralelizable, pero lanzar trabajo sin límite puede competir por CPU, disco y memoria.

## Concepto
`Get-AuditFleetSummary` acepta `-ThrottleLimit`. Con `1` ejecuta secuencialmente; con valores mayores usa `ForEach-Object -Parallel` y luego ordena el resultado para conservar salida determinista.

```powershell
Get-AuditFleetSummary -Path ./reports/*.json -ThrottleLimit 4
```

El contrato importante es que secuencial y paralelo produzcan el mismo resumen. El curso no exige que paralelo sea siempre más rápido.

## Errores comunes
- Usar `-Parallel` en cualquier pipeline por moda.
- Confundir concurrencia con garantía de menor latencia.
- Depender del orden de finalización de los workers.

## Tu turno
Ejecuta el mismo conjunto con `ThrottleLimit 1` y `2`. Compara `ReportCount`, `Critical`, `Warning` y el orden de equipos.

## Siguiente paso
Continúa con [Lección 15 — Mide antes de optimizar](15-mide-antes-de-optimizar.md).

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/foreach-object
