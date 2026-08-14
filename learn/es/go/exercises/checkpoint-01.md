# Checkpoint 01 — Timeout configurable

## Escenario

UptimeLab usa actualmente un timeout HTTP fijo de cinco segundos. Un equipo quiere ejecutar el monitor en redes distintas y necesita ajustar ese límite sin editar código.

## Tu misión

Implementa una configuración `UPTIMELAB_TIMEOUT` usando una duración válida de Go, por ejemplo `750ms`, `3s` o `1m`.

El cambio debe cumplir:

1. si la variable no existe, conserva el timeout actual de cinco segundos;
2. si existe y no puede parsearse, el proceso falla con un error claro;
3. una duración igual o menor que cero se rechaza;
4. el paquete `monitor` no debe leer variables de entorno;
5. añade al menos una prueba que proteja parsing válido y otra que proteja entrada inválida;
6. no uses `time.Sleep` para comprobar el timeout.

## Evidencia esperada

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

Después explica dónde vive la política de configuración y por qué `monitor.Checker` no debería conocer `os.Getenv`.
