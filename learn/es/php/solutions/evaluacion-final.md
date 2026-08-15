# Solución de referencia — Evaluación final PHP

> Consulta esta referencia sólo después de completar un intento serio. No existe una única solución correcta.

## Dirección de diseño

Una solución razonable trata `pending`/`confirmed` como estado de la cita, porque debe sobrevivir recarga y aparecer de forma consistente en HTML/CSV. Evita mantener colecciones separadas de pendientes y confirmadas: `Schedule` sigue siendo la fuente autoritativa.

La representación concreta puede ser un `enum` backed de PHP o una cadena validada por una abstracción pequeña. Lo importante es que el dominio impida estados imposibles y que `toArray()`/`fromArray()` conserven el contrato durable.

Para JSON legado, una ausencia del campo debe mapearse explícitamente a `pending`. Así datos existentes conservan significado sin una migración destructiva.

## Historia A — Confirmación

Una dirección coherente es añadir una operación que produzca/reemplace una cita con estado `confirmed` preservando identidad, horario, cliente y servicio. `AppointmentService` coordina cargar → construir candidato → guardar. La UI emite un POST protegido; no uses GET para mutar.

Pruebas importantes:

- nueva cita = `pending`;
- confirmación persiste y sobrevive `load`;
- JSON sin estado se rehidrata como `pending`;
- confirmar no elimina la regla de traslape;
- CSV refleja el mismo estado que la tabla.

## Historia B — Normalización

`trim()` no cubre todo whitespace Unicode. Una solución pequeña puede normalizar únicamente los extremos mediante una expresión Unicode, por ejemplo con `preg_replace` y modificador `u`, y después aplicar la regla de no-vacío.

No conviertas esta tarea en transliteración, cambio de mayúsculas o limpieza agresiva de nombres. La política debe tener un propósito concreto: impedir valores visualmente vacíos sin destruir datos válidos.

La regresión debe usar al menos un carácter de espacio Unicode que antes sobrevivía al `trim()` simple.

## Historia C — Consistencia

Conserva el flujo que ya usa AgendaPHP: cargar estado → construir candidato → validar → `save` → devolver éxito. Si `save` falla, no publiques una versión alternativa como si fuera durable.

El almacenamiento corrupto sigue siendo una falla operacional; no debe convertirse en `Schedule` vacío porque eso podría permitir reservas sobre información que en realidad existe pero no pudo leerse.

## Historia D — Frontera HTTP

La confirmación debe pasar por el mismo gate de mutaciones que create/update/cancel:

- POST;
- body dentro del límite;
- media type soportado;
- token CSRF verificado antes de dominio/persistencia.

Los headers actuales siguen siendo defense-in-depth. Para Internet real todavía faltarían, entre otros, autenticación/autorización, TLS/gestión de secretos y rate limiting/operación multi-instancia.

## Historia E — Fuentes

Ejemplos válidos de fuentes primarias:

- manual PHP de enums/type declarations para justificar representación de estado;
- manual PHP de PCRE/Unicode para la normalización;
- documentación PHPUnit para data providers/excepciones/regresiones;
- RFC 9110 para semántica HTTP.

No basta pegar links: registra qué decisión concreta respaldó cada fuente.

## Historia F — Siguiente escala

La primera frontera natural a sustituir es `AppointmentStore`. Una implementación SQLite/PDO puede conservar dominio y `AppointmentService`, migrar el JSON en una operación explícita y usar transacciones/constraints para reducir lost updates.

Un framework pasa a tener sentido cuando routing, middleware, autenticación, validación, migraciones, DI u operación web repetitiva generen suficiente presión como para que su infraestructura reduzca complejidad neta. No es requisito para que SQLite sea útil ni una respuesta automática a crecimiento.

Para multi-proceso necesitarás además coordinación de escrituras, observabilidad de errores/latencia/contención y controles de identidad/autorización si el producto deja de ser local.

## Defensa de entrevista

Una explicación fuerte distingue:

- estado de dominio de estado visual;
- fuente autoritativa de proyecciones derivadas;
- error de entrada de falla operacional;
- persistencia exitosa de estado candidato en memoria;
- hardening parcial de seguridad de seguridad productiva completa;
- una abstracción útil (`AppointmentStore`) de sobrearquitectura prematura.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y puntúa tu solución por comportamiento y explicación, no por similitud con esta referencia.
