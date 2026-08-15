# Solución de referencia — Checkpoint 01

Una dirección razonable mantiene la lectura de entorno en `cmd/uptimelab` y pasa la política ya resuelta al cliente HTTP.

Puedes extraer una función pequeña y testeable:

```go
func parseTimeout(raw string) (time.Duration, error) {
	if strings.TrimSpace(raw) == "" {
		return 5 * time.Second, nil
	}
	timeout, err := time.ParseDuration(raw)
	if err != nil {
		return 0, fmt.Errorf("UPTIMELAB_TIMEOUT: %w", err)
	}
	if timeout <= 0 {
		return 0, fmt.Errorf("UPTIMELAB_TIMEOUT must be greater than zero")
	}
	return timeout, nil
}
```

Después crea `&http.Client{Timeout: timeout}` y entrégalo a `monitor.NewChecker`.

Pruebas mínimas:

- vacío → cinco segundos;
- `750ms` → 750 ms;
- `banana` → error;
- `0s` y `-1s` → error.

La frontera importante es conceptual: `monitor` recibe un cliente que cumple `Doer`; no necesita saber si el timeout vino de ambiente, flags, archivo o una futura configuración remota. Eso mantiene el paquete reutilizable y las pruebas deterministas.

No es necesario que tu solución tenga exactamente estos nombres. Evalúa comportamiento, claridad y separación de responsabilidades.
