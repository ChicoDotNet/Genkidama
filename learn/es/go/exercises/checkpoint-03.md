# Checkpoint 03 — Señal de deterioro sin falsear la evidencia

Trabaja sobre la misma aplicación UptimeLab. No abras la solución hasta completar un intento.

## Escenario

Operaciones quiere una señal simple que identifique targets cuya disponibilidad reciente empeoró respecto de la ventana anterior. No quieren otro archivo persistido ni una base de datos nueva: la señal debe derivarse del historial existente.

## Encargo

Extiende el paquete `insights` con una función pública que reciba `[]Trend` y devuelva únicamente las tendencias con `DeltaPercent < 0`.

Requisitos:

- no mutar el slice recibido;
- conservar el orden determinista ya producido por `Trends`;
- un delta igual a cero no es deterioro;
- la función debe tener GoDoc;
- añade pruebas para delta negativo, cero y positivo;
- expón `GET /api/trends/degrading?window=N` reutilizando la misma validación `1..100`;
- el endpoint debe responder `400` ante ventana inválida y no ejecutar nuevos checks ni persistir nada.

## Evidencia esperada

```bash
cd app
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

Después demuestra con `httptest` o una ejecución local que sólo aparecen targets con delta negativo.

## Explicación obligatoria

En 3–5 párrafos responde:

1. ¿Por qué una tendencia negativa no prueba causa raíz?
2. ¿Por qué no persististe esta lista como un tercer estado?
3. ¿Qué cambiaría si el historial ya no cupiera razonablemente en memoria?
4. ¿Qué información evitarías registrar si convirtieras esta señal en alertas?

## Criterio de aprobación

La solución debe ser pequeña, determinista, probada y mantener `monitor`, `history`, `insights` y `web` como responsabilidades separadas.
