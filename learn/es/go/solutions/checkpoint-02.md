# Solución de referencia — Checkpoint 02

> Consulta esta referencia sólo después de completar un intento. No existe una única solución correcta.

## Dirección de diseño

La configuración del límite pertenece al borde de composición (`cmd/uptimelab`), porque es una decisión operativa. `history.NewLog(store, limit)` ya valida `limit > 0`, por lo que el paquete de historial conserva un contrato pequeño y reusable sin conocer variables de entorno.

Una solución razonable añade un parser pequeño para el límite, por ejemplo a partir de `UPTIMELAB_HISTORY_LIMIT`, y usa 50 como valor configurado en el escenario. El parser devuelve `(int, error)` en vez de hacer `panic`.

## Regresión crítica

La prueba más importante no verifica el texto del error: verifica el estado observable.

```go
initial := monitor.Result{Target: monitor.Target{Name: "old"}}
store := &failingStore{entries: []monitor.Result{initial}}
log, err := history.NewLog(store, 50)
if err != nil {
    t.Fatal(err)
}

err = log.Append([]monitor.Result{{Target: monitor.Target{Name: "new"}}})
if err == nil {
    t.Fatal("expected persistence failure")
}

got := log.Entries()
if len(got) != 1 || got[0].Target.Name != "old" {
    t.Fatalf("visible history changed: %+v", got)
}
```

La razón es el orden interno de `Append`: construye un candidato, llama `Save(candidate)` y sólo después asigna `l.entries = candidate`.

## Retención

La política debe conservar las entradas más nuevas:

```text
[one, two, three] con limit=2 → [two, three]
```

No ordenes por timestamp durante la inserción: UptimeLab ya conserva el orden de observación del lote. Si más adelante se importan fuentes fuera de orden, esa será otra decisión explícita.

## Por qué el handler devuelve 503

`RunChecks` representa una operación completa: ejecutar y registrar. Si la segunda mitad falla, devolver 200 afirmaría éxito parcial sin indicarlo. `503 Service Unavailable` comunica una falla operativa temporal sin convertirla en error de validación del target.

## Lo que no resuelve

- coordinación entre dos procesos que escriben el mismo archivo;
- almacenamiento eficiente para millones de observaciones;
- retries/backoff;
- tolerancia a caída del proceso durante una escritura.

Esos límites deben reconocerse antes de elegir una base de datos o una cola.

Vuelve a la [Lección 08](../lessons/08-estado-consistente-y-checkpoint.md) y conserva `go test -race ./...` verde.
