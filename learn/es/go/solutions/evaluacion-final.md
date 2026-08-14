# Solución de referencia — Evaluación final Go

> Consulta esta referencia sólo después de completar un intento. No existe una única solución correcta.

## Dirección de diseño

Una solución razonable mantiene la capacidad de habilitar/deshabilitar targets cerca del modelo/configuración de targets, no dentro de `web` ni de `history`. El checker recibe únicamente targets habilitados o conoce un campo explícito cuya semántica pueda probarse sin I/O adicional.

Para compatibilidad, una configuración anterior sin el nuevo dato debe interpretarse como **habilitada**. Evita una migración que convierta silenciosamente targets existentes en deshabilitados.

## Historia A — Target deshabilitado

Una representación posible es un booleano explícito cuya ausencia tenga default compatible. La prueba importante no es comprobar el campo, sino demostrar que el servidor de prueba del target deshabilitado recibe **cero requests** y que las vistas derivadas no lo incluyen en la muestra activa.

No persistas summary/trends para resolver esto: siguen siendo derivados del historial.

## Historia B — Duplicados normalizados

Una dirección razonable normaliza nombres con `strings.TrimSpace` y comparación case-insensitive antes de aceptar la configuración. Puedes usar una clave normalizada, por ejemplo `strings.ToLower`, siempre que documentes la decisión.

El contrato esperado es rechazo temprano: dos targets que para el operador representan el mismo nombre no deben llegar al scheduler/checker como configuración ambigua.

La regresión debe incluir al menos un caso equivalente a `"API"` y `" api "`.

## Historia C — Consistencia

No muevas la mutación durable después de la visible. El patrón correcto sigue siendo construir candidato → persistir → publicar en memoria.

Si tu cambio toca operaciones cancelables, conserva `context.Context` y permite reconocer `context.Canceled`/`context.DeadlineExceeded` con `errors.Is` cuando corresponda. No conviertas cancelación en un resultado HTTP exitoso del target.

## Historia D — Concurrencia

El límite de concurrencia existente sigue siendo valioso: miles de targets no deberían producir miles de requests simultáneas por accidente. Una evolución para gran escala podría usar workers, rate limits por host, backpressure y scheduling distribuido, pero no hace falta introducirlos para aprobar esta evaluación.

## Historia E — Fuentes oficiales

Ejemplos válidos:

- `context` en `pkg.go.dev/context` para justificar propagación de cancelación y deadlines;
- `net/http` para contratos de cliente/servidor;
- documentación del race detector para explicar qué carreras puede encontrar durante ejecución de pruebas;
- `errors` para wrapping y `errors.Is`.

Lo importante es enlazar la fuente oficial y explicar la decisión concreta que sustentó.

## Historia F — Multi-instancia

La primera frontera natural a sustituir es `history.Store`. Una implementación compartida necesitaría control de concurrencia —transacciones, optimistic concurrency/versionado o mecanismo equivalente— para evitar lost updates.

`monitor`, las reglas de configuración y `insights` deberían seguir independientes del motor durable. La observabilidad tendría que incorporar identidad de instancia, colas/reintentos y errores de almacenamiento sin registrar URLs o datos sensibles innecesarios. También aparecerían autenticación/autorización y protección del almacenamiento compartido como riesgos nuevos.

## Defensa de entrevista

Una respuesta fuerte distingue:

- **concurrencia** de paralelismo ilimitado;
- **resultado HTTP** de **error de transporte**;
- **estado durable** de **vistas derivadas**;
- **diagnóstico agregado** de logging detallado;
- **hardening acotado** de seguridad completa;
- consistencia de una instancia de coordinación multi-proceso.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y puntúa tu solución por comportamiento y explicación, no por similitud con esta referencia.