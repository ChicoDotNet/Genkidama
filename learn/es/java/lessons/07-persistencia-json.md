# Lección 07 — Persistencia JSON detrás de una frontera

## Qué vas a conseguir

Vas a hacer que HelpDesk sobreviva un reinicio sin acoplar el dominio a archivos, Jackson o rutas del sistema.

## Antes de empezar

Completa la [Lección 06](06-modificar-prioridad.md).

## El problema

Hasta ahora cerrar el proceso borra todos los tickets. La necesidad de persistencia ya es observable. Resolverla metiendo `Files.writeString` dentro de `TicketService` mezclaría reglas de negocio con I/O.

## Concepto

`TicketStore` define dos operaciones pequeñas: cargar y reemplazar un snapshot completo. `InMemoryTicketStore` mantiene pruebas rápidas; `JsonFileTicketStore` usa Jackson y archivos locales. El dominio depende de la interfaz, no del formato.

## Demostración

[EJECUTAR] Inicia HelpDesk, crea dos tickets, detén el proceso y vuelve a iniciarlo. El archivo por defecto es:

```text
data/tickets.json
```

Puedes aislar una ejecución:

```bash
HELPDESK_DATA_FILE=/tmp/helpdesk-demo.json mvn exec:java -Dexec.mainClass=io.genkidama.learn.java.helpdesk.HelpDeskApplication
```

En PowerShell configura primero `$env:HELPDESK_DATA_FILE`.

## Código real

La aplicación compone la frontera:

```java
var store = new JsonFileTicketStore(json, dataFile);
var server = new HelpDeskHttpServer(new TicketService(store), json, port);
```

`TicketService` restaura el estado y calcula el siguiente ID a partir del máximo persistido. También rechaza IDs duplicados y datos que violen invariantes.

## Qué acaba de pasar

Ahora la aplicación tiene persistencia real, pero las reglas de tickets siguen siendo ejecutables con memoria. Cambiar JSON por otra tecnología no exige reescribir `Ticket` ni el servidor HTTP.

## Errores comunes

- Hacer del archivo JSON una dependencia del dominio.
- Tratar JSON corrupto como lista vacía y perder evidencia del problema.
- Reiniciar IDs en 1 después de cargar datos existentes.
- Guardar secretos o información sensible en fixtures de aprendizaje.

## Buenas prácticas

Una primera ejecución sin archivo es normal; un archivo existente corrupto no lo es. Diferencia ambos casos y falla con diagnóstico explícito.

## Tu turno

[PAUSA PARA EJERCICIO] Guarda dos tickets, crea un servicio nuevo sobre el mismo archivo y demuestra con una prueba que el tercer ticket recibe ID 3.

## Cómo comprobar

```bash
mvn verify
```

Inspecciona además un archivo temporal generado por tu prueba manual; no edites el archivo mientras el proceso escribe.

## Solución enlazada

El [Checkpoint 02](../exercises/checkpoint-02.md) integra persistencia y fallos; no abras la solución antes del intento.

## Reto adicional

¿Qué ventajas y límites tiene reemplazar un snapshot completo frente a una base de datos? Piensa en atomicidad, concurrencia y tamaño antes de elegir tecnología.

## Resumen

- `TicketStore` aísla persistencia.
- JSON resuelve la necesidad actual sin añadir otra dependencia.
- Estado corrupto falla explícitamente.
- IDs continúan después de reiniciar.

## Siguiente paso

Continúa con [Lección 08 — Persistir antes de publicar + Checkpoint 02](08-persistencia-segura-y-checkpoint.md).

## Referencias

- [Files — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/file/Files.html)
- [Path — Java SE 25](https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/file/Path.html)
- [Jackson databind](https://github.com/FasterXML/jackson-databind)
