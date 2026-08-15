# Checkpoint 02 — Historial durable sin estado fantasma

Trabaja sobre UptimeLab después de la lección 08. No abras la solución hasta terminar un intento.

## Escenario

Tu equipo quiere conservar sólo las últimas **50** observaciones en una instalación pequeña y necesita evidencia de que una falla de persistencia nunca altera el historial visible.

## Tareas

1. Haz configurable el límite de historial desde la composición de la aplicación sin leer variables de entorno dentro de `history`.
2. Valida que el límite sea mayor que cero y produce un error útil si no lo es.
3. Añade una prueba donde el store ya contenga una entrada, falle al guardar una segunda y se demuestre que `Entries()` conserva exactamente la primera.
4. Añade una prueba de retención: al insertar más resultados que el límite, sobreviven únicamente los más recientes y en orden.
5. Mantén `go test -race ./...` verde.

## Restricciones

- No expongas el slice interno del historial.
- No ignores errores de `Save`.
- No metas filesystem ni `os.Getenv` dentro de `monitor`.
- No uses `panic` para configuración inválida del usuario.
- No añadas una base de datos sólo para resolver este checkpoint.

## Evidencia esperada

```bash
cd app
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

Explica además por qué “el check HTTP terminó” no es suficiente para responder 200 si el contrato de la operación incluye registrar el resultado.

## Criterio de terminado

El checkpoint está terminado cuando puedes demostrar con pruebas que la política de retención es determinista y que una escritura fallida no publica un snapshot que no existe durablemente.

Después de intentarlo, consulta [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).
