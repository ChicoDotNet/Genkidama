# Evaluación final — PHP / AgendaPHP

Resuelve este encargo sin una receta de archivos o funciones. Puedes consultar documentación oficial y las lecciones, pero no abras la solución hasta terminar un intento serio.

## Historia A — Estado de confirmación

El negocio necesita distinguir citas `pending` y `confirmed`.

Requisitos observables:

- una cita nueva comienza `pending`;
- puede confirmarse mediante una mutación explícita;
- el estado sobrevive guardar/cargar JSON;
- una cita confirmada sigue participando en las mismas reglas de traslape que una pendiente;
- tabla y CSV muestran el estado desde la misma fuente autoritativa;
- un JSON legado sin campo de estado continúa cargando con un default compatible y documentado.

Escribe primero pruebas que protejan el comportamiento elegido.

## Historia B — Bug de normalización

Asume que hoy el sistema puede aceptar nombres de cliente o servicio compuestos sólo por espacios Unicode/no-break spaces que `trim()` simple no normaliza como esperaría una persona usuaria.

Define una política pequeña y explícita para normalizar/rechazar entrada visualmente vacía. Corrige el defecto sin convertir el dominio en un sanitizador general y añade una regresión que hubiera fallado antes.

## Historia C — Fallas y consistencia

Conserva estos contratos:

- una falla al guardar no publica un estado candidato como durable;
- un JSON inválido no se interpreta como agenda vacía;
- una entrada inválida sigue siendo 422 y una falla durable 503;
- una mutación nueva debe respetar media type, límite de body y CSRF antes de tocar estado.

Demuestra al menos uno con una prueba nueva ligada a tu cambio.

## Historia D — HTTP y seguridad acotada

Expón la confirmación mediante el flujo web existente sin introducir GET mutador. Reutiliza la defensa CSRF y conserva los headers defensivos actuales.

Explica por qué esto no convierte AgendaPHP en una aplicación lista para Internet y nombra al menos tres controles que seguirían faltando para ese escenario.

## Historia E — Documentación oficial

Consulta al menos dos fuentes primarias/oficiales y deja una nota breve indicando qué decisión sustentaron. Una debe ser de PHP; la otra puede ser PHP, PHPUnit, Composer o una especificación HTTP pertinente.

## Historia F — Diseño de siguiente escala

Sin implementarlo, diseña la migración de almacenamiento si AgendaPHP necesitara múltiples procesos o consultas selectivas sobre miles de citas. Identifica:

- qué interfaz sustituirías primero;
- cómo migrarías el JSON existente;
- cómo evitarías lost updates/escrituras concurrentes;
- qué reglas permanecerían en dominio/aplicación;
- qué observabilidad y seguridad nuevas necesitarías.

No respondas automáticamente “Laravel”: compara al menos una opción de SQLite/PDO con una opción de framework y explica qué presión justificaría cada una.

## Evidencia mínima

Entrega:

```bash
bash tools/verify.sh
bash tools/smoke.sh
```

Además muestra:

1. prueba de creación pendiente y confirmación durable;
2. prueba de compatibilidad con JSON legado;
3. regresión de normalización;
4. una prueba de falla/consistencia/CSRF asociada al cambio;
5. flujo manual o smoke que confirme la cita y la vuelva a leer;
6. dos referencias oficiales consultadas;
7. defensa de arquitectura de aproximadamente cinco minutos.

Autoevalúate con [`rubrica-final.md`](rubrica-final.md).
