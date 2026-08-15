# Lección 07 — Una frontera de persistencia

## Qué vas a conseguir

Separarás el estado de la forma concreta de guardarlo mediante `AppStateStore`, de modo que HTTP pueda persistir sin enseñar al dominio qué es un archivo.

## Antes de empezar

Completa la [Lección 06](06-transiciones-de-proyecto-y-api.md).

## El problema

Reiniciar el servidor borra clientes, cotizaciones y proyectos. Podríamos llamar `writeFile` desde cada ruta, pero entonces el transporte, el formato de almacenamiento y las reglas quedarían acoplados.

## Concepto

Una interfaz pequeña puede representar una capacidad:

```ts
interface AppStateStore {
  load(): Promise<AppSnapshot>;
  save(snapshot: AppSnapshot): Promise<void>;
}
```

El servidor depende de esa capacidad, no de JSON. Las pruebas HTTP pueden inyectar un `CaptureStore`; producción usa `JsonFileStateStore`.

Esto no es “usar interfaces porque TypeScript tiene interfaces”. La abstracción aparece porque ya existen dos necesidades concretas: persistencia real y pruebas sin tocar disco.

## Demostración

[EN PANTALLA] Sigue una creación de proyecto:

1. HTTP valida cliente y datos;
2. dominio crea el proyecto;
3. estado en memoria cambia;
4. el store recibe un snapshot;
5. la respuesta HTTP se envía.

## Código real

`snapshotState` copia los arreglos antes de cruzar la frontera. El store recibe un valor serializable y no acceso directo a las colecciones mutables del servidor.

## Qué acaba de pasar

La aplicación dejó de asumir que memoria es persistencia, sin convertir al dominio en una capa de infraestructura.

## Errores comunes

- Crear una interfaz para cada clase aunque sólo exista una implementación y ningún límite real.
- Pasar `AppState` mutable completo al adaptador y permitir que éste lo modifique.
- Hacer pruebas HTTP contra el archivo real del usuario.
- Atrapar errores de persistencia y fingir que el guardado funcionó.

## Buenas prácticas

Las fronteras deben ser pequeñas, nombrar capacidades y hacer visibles los fallos. Inyecta dependencias donde ayudan a probar comportamiento, no para presumir arquitectura.

## Tu turno

Extiende `CaptureStore` para comprobar que una transición inválida no provoca una escritura adicional.

## Cómo comprobar

```bash
npm run verify
```

## Solución enlazada

La referencia completa del bloque está en [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md), después del intento.

## Reto adicional

Imagina un store SQLite. ¿Qué archivos del dominio deberían cambiar? Una respuesta fuerte es: ninguno.

## Resumen

`AppStateStore` separa capacidad de almacenamiento de su implementación y hace testeable el efecto de persistir.

## Siguiente paso

Continúa con [Lección 08 — JSON confiable y Checkpoint 02](08-json-confiable-y-checkpoint.md).

## Referencias

- [TypeScript — Interfaces](https://www.typescriptlang.org/docs/handbook/2/objects.html)
- [Node.js File system promises](https://nodejs.org/api/fs.html#promises-api)
