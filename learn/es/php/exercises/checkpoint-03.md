# Checkpoint 03 — Un reporte diario debe ser consistente

Trabaja sobre AgendaPHP después de la Lección 12. No abras la solución antes de completar un intento.

## Escenario

Una persona necesita compartir la agenda de un día con otro equipo. El reporte visible y el CSV deben representar exactamente la misma consulta, incluso cuando además se filtra por servicio. Un error de fecha no debe producir un reporte parcial y un archivo durable corrupto no debe fingir que la agenda está vacía.

## Encargo

Implementa y demuestra estos comportamientos:

1. Crea al menos tres citas: dos el mismo día y una en otro día.
2. Filtra por el primer día y confirma que la segunda fecha no aparece.
3. Combina el día con un texto de servicio y verifica que el conteo y los minutos reservados corresponden sólo al subconjunto visible.
4. Exporta CSV con esos mismos filtros. Las filas del CSV deben corresponder al mismo conjunto y orden que la tabla.
5. Usa un servicio que contenga una coma y demuestra que el CSV sigue siendo válido.
6. Prueba una fecha imposible como `2026-02-31`: la respuesta debe ser 422 y no debe modificar el archivo de datos.
7. En una copia descartable del archivo, introduce JSON inválido: la aplicación debe responder 503, no 200 con agenda vacía.
8. Añade al menos una regresión automatizada que proteja una de las reglas anteriores.

## Restricciones

- No persistas filtros, conteos ni minutos derivados.
- No implementes escaping CSV manual con concatenación de comas.
- No cambies la semántica de create/update/cancel.
- No captures cualquier `Throwable` para convertir todos los problemas en la misma respuesta.
- No migres a SQLite sólo para completar el checkpoint; explica qué necesidad real justificaría esa migración.

## Evidencia mínima

```bash
cd app
composer test
bash tools/smoke.sh
```

Además explica:

- por qué el rango de día es `[inicio, siguiente medianoche)`;
- por qué CSV vive fuera del dominio;
- por qué corrupción durable produce 503 y no un calendario vacío;
- qué limitación single-process conserva JSON.

## Criterio de aceptación

El checkpoint está completo cuando tabla, resumen y CSV comparten una única proyección derivada, los errores no mutan estado y puedes defender dónde termina el dominio y empiezan HTTP/filesystem/formatos.
