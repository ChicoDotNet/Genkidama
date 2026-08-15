# Checkpoint 04 — Android offline first

Llegaste al punto en que FieldFlow debe poder explicarse como una app Android profesional, no sólo como un ejercicio Kotlin/JVM.

## Encargo
Sin modificar el núcleo por conveniencia del framework, diseña el slice Android que:

1. implementa `WorkOrderRepository` mediante Room;
2. mapea entidad Room ↔ `WorkOrder`;
3. expone `Loading`, `Ready` y `Error` como estado observable;
4. renderiza una lista Compose y eleva la intención de completar una orden;
5. permite operar sin red;
6. registra cambios pendientes y define una estrategia de reintento/conflicto.

## Restricciones
- `WorkOrder` no recibe anotaciones Room.
- `WorkOrderService` no importa Android, Compose, Room ni HTTP.
- La UI no llama al DAO directamente.
- No conviertas un error de almacenamiento o sincronización en una lista vacía.
- No asumas que “última escritura gana” es una política universalmente segura.

## Evidencia esperada
Entrega un diagrama de dependencias, firmas de entidad/DAO/repositorio/ViewModel/estado y al menos cuatro escenarios de prueba: lectura Room, mapping, transición de estado de UI y operación offline pendiente de sincronización.

## Autoevaluación
Puedes avanzar si explicas qué código seguiría siendo prueba JVM, qué requiere Room/Android y por qué una pantalla Compose puede probarse con estado fabricado.

## Siguiente paso
Continúa con [17 — Evaluación final: entrega FieldFlow](17-evaluacion-final.md).
