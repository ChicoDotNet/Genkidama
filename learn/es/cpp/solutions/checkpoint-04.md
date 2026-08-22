# Solución de referencia — Checkpoint 04

La solución del curso modela tres ideas separadas:

- `DiscoveryOptions` lleva `std::stop_token` y una callback de progreso;
- `ControlledDiscoveryReport` devuelve registros parciales, métricas y `cancelled`;
- `DiscoveryContext` encapsula atomics y serializa únicamente la callback, no el vector principal de resultados.

El caso de filesystem mutable elimina un archivo desde la callback antes de pedir `file_size`. La operación devuelve `error_code`, incrementa `entries_skipped` y continúa. Una raíz inválida, en cambio, sigue siendo error fatal porque no existe una operación útil que recuperar.

`compare_discovery` mide ambos caminos y prueba equivalencia, pero no afirma que paralelo deba ganar. El workflow completa la evidencia con GCC, Clang y MSVC.

La idea importante no es copiar estas clases: es conservar cancelación cooperativa, diagnóstico explícito, mínimo estado compartido y validación multiplataforma.
