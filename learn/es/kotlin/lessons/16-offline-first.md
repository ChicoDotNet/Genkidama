# 16 — Diseña FieldFlow offline first

## Qué vas a conseguir
Definir una estrategia de sincronización donde la app siga siendo útil sin conectividad y los conflictos sean decisiones explícitas.

## Antes de empezar
Completa las lecciones 13–15. La persistencia local y la UI deben tener fronteras claras.

## El problema
Una app de trabajo en campo no puede asumir conexión permanente. “Si falla Internet, muestra error” no es offline first: sólo es una app online con un mensaje de fallo.

## Concepto
En FieldFlow, Room puede ser la fuente local que la UI observa. Las acciones del usuario se confirman localmente y se registran como pendientes de sincronización. Un proceso separado intenta enviar cambios cuando existe conectividad.

Cada cambio remoto necesita una política observable: éxito, reintento, rechazo o conflicto. No existe una política universal de “última escritura gana” que sea correcta para todo dominio.

## Demostración
[DEMO] Imagina que un técnico completa `WO-42` sin señal mientras otro usuario cambia su prioridad desde el servidor. Escribe tres políticas posibles y qué información perdería cada una.

## Código real
El núcleo actual ya demuestra que una operación puede persistirse localmente antes de que exista Android. La evolución natural es conservar esa semántica y añadir metadata de sincronización en el adaptador Android, no convertir `WorkOrderService` en cliente HTTP.

## Qué acaba de pasar
Offline dejó de ser una excepción de UI y se convirtió en una propiedad arquitectónica: almacenamiento local autoritativo para la experiencia inmediata, sincronización explícita y conflictos visibles.

## Errores comunes
- bloquear la edición cuando no hay red;
- borrar cambios locales después de un HTTP 500;
- reintentar infinitamente errores no recuperables;
- resolver conflictos silenciosamente;
- mezclar DTO remoto, entidad Room y modelo de dominio en una sola clase.

## Buenas prácticas
Usa identificadores estables, operaciones idempotentes cuando sea posible, backoff para fallos transitorios y estados de sincronización observables.

## Tu turno
Diseña la metadata mínima para distinguir `synced`, `pending`, `failed` y `conflict`. Decide qué debe ver el usuario en cada estado.

## Cómo comprobar
Desconecta la red conceptualmente: crear/completar órdenes sigue funcionando localmente. Al reconectar, ninguna operación desaparece sin una resolución explícita.

## Solución enlazada
Revisa [`FileWorkOrderRepository.kt`](../app/src/main/kotlin/dev/genkidama/fieldflow/FileWorkOrderRepository.kt): su persistencia local durable es la semilla conceptual que Room reemplaza en Android; la sincronización es una responsabilidad adicional, no una excusa para eliminar la frontera.

## Reto adicional
Propón una clave de idempotencia para enviar una transición de estado al servidor y explica cuánto tiempo debería conservarse.

## Resumen
Una aplicación offline first confirma localmente, sincroniza después y hace visibles los conflictos importantes.

## Siguiente paso
Realiza [Checkpoint 04 — Android offline first](checkpoint-04.md) y después enfrenta la evaluación final.

## Referencias
- https://developer.android.com/topic/architecture/data-layer/offline-first
- https://developer.android.com/topic/libraries/architecture/workmanager
