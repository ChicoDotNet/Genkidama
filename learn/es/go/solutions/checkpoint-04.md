# Solución de referencia — Checkpoint 04

Una solución razonable mantiene el diagnóstico completamente en `web` y usa `RequestMetrics` como collector agregado.

## Reloj determinista

Inyecta un reloj que avance una cantidad fija en cada llamada. Así una petición puede producir, por ejemplo, `25 ms` sin depender del scheduler del runner ni de `time.Sleep`.

## Falla observable

Usa un `BatchChecker` falso que devuelva error. `/api/checks` debe responder 503. Después `/api/diagnostics` debe reportar una petición, una falla y la duración controlada.

## Privacidad

El target de la prueba puede llamarse `private-api` y apuntar a una URL ficticia sensible. Serializa el diagnóstico y comprueba que ninguna de esas cadenas aparece. `MetricsSnapshot` sólo necesita:

- `requests`;
- `failures`;
- `total_duration_ms`.

No agregues URL, path, query string, body ni nombre del target sólo para facilitar debugging.

## Opt-in y hardening

Construido con `NewServer`, `/api/diagnostics` devuelve 404. Una ruta desconocida también devuelve 404. La respuesta de `/health` conserva `nosniff`, `no-referrer` y CSP.

## Verificación

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

La referencia prioriza el contrato observable. Tu implementación puede organizar helpers de otra forma si mantiene separación, concurrencia segura y ausencia de datos sensibles.
