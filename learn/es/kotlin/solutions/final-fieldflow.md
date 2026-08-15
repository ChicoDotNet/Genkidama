# Solución de referencia — Mantén y evoluciona FieldFlow

Esta referencia describe una solución profesional posible. Compárala con tu entrega después de intentar el ejercicio; no la trates como una receta obligatoria.

## 1. Prioridad CRITICAL

Añade `CRITICAL` al modelo de prioridad y centraliza el orden en una regla explícita. Las pruebas deben demostrar al menos `CRITICAL > HIGH > MEDIUM > LOW` y conservar el comportamiento de órdenes persistidas con valores anteriores.

Una solución sólida evita repartir comparaciones numéricas mágicas por servicios, repositorios y UI. La prioridad conoce o expone su precedencia; los consumidores sólo la usan.

## 2. Persistencia corrupta

`FileWorkOrderRepository` debe distinguir entre “archivo válido sin órdenes” y “archivo ilegible/corrupto”. Devuelve o lanza un fallo explícito en la frontera de almacenamiento y protege el caso con una prueba que escriba contenido inválido antes de leer.

No conviertas una excepción de parseo en `emptyList()`: hacerlo puede ocultar pérdida de datos como si fuera un estado de negocio legítimo.

## 3. Órdenes abiertas por prioridad

Una opción razonable es mantener en el repositorio una operación simple que devuelve las órdenes disponibles y colocar la regla de “abiertas + orden de negocio” en `WorkOrderService`. Eso conserva la persistencia intercambiable y permite probar la regla sin Room ni archivos.

Si eliges que el repositorio filtre/ordene por eficiencia, conserva el contrato en términos del dominio y añade una prueba de contrato para que las implementaciones in-memory, archivo y Room produzcan la misma semántica.

## 4. Room

Amplía `WorkOrderEntity` y el mapper, no `WorkOrder`, con los detalles necesarios de almacenamiento. Para la consulta, un DAO puede exponer una query que filtre `completed = 0`; la traducción de `priority` debe seguir validándose al cruzar la frontera.

El dominio no debe importar `androidx.room.*`.

## 5. Estado de UI

Modela el estado de forma que los datos disponibles y la actividad transitoria sean dimensiones independientes. Por ejemplo, conserva la lista de órdenes y añade una propiedad como `isSaving`/`syncStatus` en vez de reemplazar toda la pantalla por un estado `Loading` que borre visualmente datos útiles.

Compose debe renderizar el estado y emitir intenciones; la lógica de guardado o sincronización pertenece fuera de los composables.

## 6. Conflicto offline

No sobrescribas silenciosamente la cancelación remota con la finalización local. Registra ambos hechos y produce un conflicto visible o una regla de resolución explícita. Una política defendible puede conservar `cancelled` como estado remoto autoritativo y registrar la finalización local como operación rechazada que requiere revisión.

La decisión correcta depende del negocio; lo importante es que sea determinística, observable e idempotente al reintentar.

## Pruebas de referencia

La suite debería cubrir, como mínimo:

- precedencia de `CRITICAL`;
- compatibilidad con prioridades existentes;
- lectura corrupta produce error explícito;
- consulta excluye completadas y respeta precedencia;
- un reintento de la misma operación no duplica efectos cuando aplique.

Mantén el criterio del repositorio: coverage >=44% es suficiente cuando es medible y protege comportamiento relevante; no agregues tests cosméticos para perseguir 100%.

## Evidencia Android

Después de modificar Room/Compose, valida desde `android/`:

```bash
gradle :app:assembleDebug :app:testDebugUnitTest
```

Si cambias una interacción que requiere dispositivo/emulador, añade la prueba Android correspondiente en lugar de afirmar que el build unitario demuestra comportamiento que no ejecutó.

## Qué comparar con tu solución

- ¿El dominio siguió libre de Android?
- ¿Los errores de datos siguen siendo distinguibles de estados legítimos?
- ¿La regla de prioridad vive en un solo lugar comprensible?
- ¿Tu UI conserva datos mientras guarda/sincroniza?
- ¿El conflicto offline tiene una política explícita?
- ¿Puedes defender una simplificación que elegiste deliberadamente?
