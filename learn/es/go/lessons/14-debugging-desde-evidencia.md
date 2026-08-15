# Lección 14 — Debugging desde evidencia

## Qué vas a conseguir

Aprenderás a reducir un fallo de UptimeLab con pruebas dirigidas, errores explícitos y observaciones reproducibles antes de modificar código.

## Antes de empezar

Completa la [Lección 13](13-gate-profesional-de-go.md).

## El problema

Una respuesta 503 en `/api/checks` puede venir de red, cancelación o persistencia. Cambiar código antes de identificar la frontera que falló suele crear más ruido.

## Concepto

Depurar es reducir hipótesis. Usa el mensaje de error, una prueba pequeña y la frontera responsable. Go conserva contexto con `%w`, lo que permite añadir significado sin perder la causa original.

## Demostración

[DEMO] Ejecuta sólo la regresión de persistencia:

```bash
go test ./web -run TestChecksEndpointDoesNotPublishHistoryWhenPersistenceFails -v
```

Después ejecuta las pruebas de `history` y compara qué capa conoce disco y cuál sólo traduce el error a HTTP.

## Código real

`RunChecks` no silencia `history.Append`: devuelve `web: persist history: ...`. La API traduce ese fallo operacional a 503 y la prueba confirma que el estado visible anterior permanece intacto.

## Qué acaba de pasar

Separaste síntoma HTTP, causa de persistencia e invariante de estado. El diagnóstico produjo una hipótesis verificable en lugar de una edición especulativa.

## Errores comunes

- Agregar `fmt.Println` por todo el dominio.
- Registrar URLs o payloads sensibles para “tener más contexto”.
- Corregir el status HTTP sin proteger la consistencia.
- Ejecutar toda la suite cuando una prueba dirigida responde primero la pregunta.

## Buenas prácticas

Empieza por la prueba más pequeña que reproduce el fallo; amplía el radio sólo después. Mantén errores accionables y datos sensibles fuera de logs/diagnóstico.

## Tu turno

[PAUSA PARA EJERCICIO] Fuerza un `Store.Save` fallido con el fake existente, sigue la cadena de error y documenta qué paquete debe cambiar si el contrato deseado fuera distinto.

## Cómo comprobar

```bash
go test ./web -run Persistence -v
go test ./history -v
go test -race ./...
```

## Solución enlazada

La solución esperada es el razonamiento: persistencia falla en `history`, `web` conserva la invariante y traduce la falla; no hace falta mover I/O al dominio.

## Reto adicional

Investiga `errors.Is`/`errors.As` y describe cuándo un error tipado mejoraría la traducción HTTP.

## Resumen

Debugging profesional comienza por evidencia y fronteras, no por intuición.

## Siguiente paso

Continúa con [medición antes de optimizar](15-medir-antes-de-optimizar.md).

## Referencias

- https://go.dev/doc/diagnostics
- https://pkg.go.dev/errors
- https://pkg.go.dev/fmt
