# Solución de referencia — Checkpoint 03

> Consulta esta referencia sólo después de intentar el checkpoint.

Una solución razonable mantiene la selección de deterioro dentro de `insights` porque trabaja sobre una vista derivada ya tipada y no necesita HTTP ni persistencia.

```go
// Degrading returns trends whose recent availability is lower than the previous window.
func Degrading(trends []Trend) []Trend {
    result := make([]Trend, 0, len(trends))
    for _, trend := range trends {
        if trend.DeltaPercent < 0 {
            result = append(result, trend)
        }
    }
    return result
}
```

La prueba importante no es sólo “incluye -10”. También protege que `0` y valores positivos queden fuera y que el orden se conserve. Al crear un slice nuevo, la función no reutiliza el backing array del caller para escribir resultados.

En `web`, evita duplicar parsing de `window`: extrae una pequeña función privada que convierta el query param y aplique el rango `1..100`. El handler de deterioro debe hacer únicamente:

```text
snapshot historial
→ Trends(snapshot, window)
→ Degrading(trends)
→ JSON
```

No llama `RunChecks`, por lo que consultar diagnóstico no altera el sistema observado.

Una tendencia negativa es una señal, no una causa raíz: indica que la muestra reciente tiene menor disponibilidad que la anterior. Logs de la aplicación monitoreada, traces, despliegues, infraestructura y otras fuentes son necesarias para explicar por qué.

No se persiste la lista de deterioro porque puede reconstruirse del historial. Si el historial creciera a millones de filas, el contrato de `history.Store` tendría que evolucionar hacia consultas agregadas o ventanas ejecutadas por el almacenamiento; eso no obliga a contaminar `monitor` con SQL u otra tecnología concreta.

Si la señal se convirtiera en alertas, evita incluir secretos, tokens, URLs privadas completas, payloads o datos personales. Identifica targets con un nombre operativo deliberadamente no sensible y registra sólo los datos necesarios para investigar.
