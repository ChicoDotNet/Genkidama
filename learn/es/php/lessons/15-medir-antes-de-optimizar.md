# Lección 15 — Medir antes de optimizar

## Qué vas a conseguir

Vas a razonar sobre rendimiento con una carga representativa antes de introducir caché, base de datos o complejidad que todavía no necesitas.

## Antes de empezar

Completa la [Lección 14](14-debugging-desde-evidencia.md).

## El problema

AgendaPHP recorre una colección en memoria para filtrar por fecha/servicio y calcular minutos. Es tentador afirmar que “necesita SQLite” sólo porque una base de datos parece más profesional.

## Concepto

Optimizar exige una señal medible y una hipótesis. Para esta app interesa separar al menos:

- tiempo de cargar/decodificar el JSON;
- tiempo de proyectar filtros/resumen;
- tiempo de guardar el documento completo;
- tamaño y cantidad de citas representativas.

No midas una única ejecución fría y la conviertas en conclusión. Repite, usa datos sintéticos y compara escenarios equivalentes.

## Demostración

[DEMO] Genera en una copia de laboratorio decenas, cientos y miles de citas ficticias no solapadas. Mide el tiempo total de una consulta y una escritura con herramientas del sistema o `hrtime(true)` en un script temporal. No dejes instrumentación de laboratorio mezclada con la aplicación si no aporta una capacidad operativa real.

## Código real

[`Schedule`](../app/src/Domain/Schedule.php) conserva proyecciones puras; [`JsonAppointmentStore`](../app/src/Infrastructure/JsonAppointmentStore.php) concentra filesystem/JSON. Esa separación permite medir dónde está el costo antes de cambiar arquitectura.

## Qué acaba de pasar

Una medición puede justificar SQLite más adelante, pero no lo presupone. Si el costo dominante es reescribir un documento grande, una persistencia con actualizaciones selectivas puede resolver un problema real. Si la agenda tiene 30 citas, la migración puede costar más complejidad que el beneficio.

## Errores comunes

- Optimizar por intuición o moda.
- Medir con datos personales reales.
- Comparar implementaciones con cargas distintas.
- Introducir caché sin una política de invalidación.
- Confundir microbenchmark con experiencia end-to-end.

## Buenas prácticas

Define primero la pregunta: “¿qué operación es demasiado lenta, bajo qué tamaño y para quién?”. Guarda resultados de laboratorio en tus notas, no como promesa universal de rendimiento.

## Tu turno

Escribe un plan de benchmark con tres tamaños de agenda, dos operaciones y un umbral que justificaría investigar otra persistencia. Explica qué variable mantendrías constante.

## Cómo comprobar

El gate funcional debe seguir verde después de cualquier experimento:

```bash
bash tools/verify.sh
bash tools/smoke.sh
```

## Solución enlazada

No hay un número correcto universal. Una respuesta defendible declara tamaño, entorno, operación, repeticiones y criterio de decisión.

## Reto adicional

Explica cómo cambiaría tu benchmark si existieran dos procesos escritores simultáneos. Rendimiento y consistencia son problemas distintos.

## Resumen

- Mide antes de optimizar.
- Separa I/O, proyección y escritura para formular hipótesis.
- Usa datos sintéticos y escenarios repetibles.
- SQLite entra cuando resuelve un problema observado, no como adorno.

## Siguiente paso

Continúa con [Lección 16 — Hardening HTTP + Checkpoint 04](16-hardening-http-y-checkpoint-04.md).

## Referencias

- [PHP — hrtime](https://www.php.net/manual/en/function.hrtime.php)
- [PHP — Performance considerations](https://www.php.net/manual/en/features.gc.performance-considerations.php)
