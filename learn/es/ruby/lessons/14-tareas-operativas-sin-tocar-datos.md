# Lección 14 — Tareas operativas sin tocar datos

## Qué vas a conseguir

Usarás una tarea Rails para inspeccionar ContactDesk desde terminal sin convertir mantenimiento en una colección de comandos peligrosos.

## El problema

Operaciones necesita comprobar si la app puede hablar con la base y cuántos contactos existen. Abrir consola y escribir SQL ad hoc aumenta riesgo y no deja un contrato repetible.

## Concepto

Una tarea en `lib/tasks` carga el entorno y delega a código de aplicación probado. Nuestra tarea `contactdesk:diagnostics` es deliberadamente **read-only**: reporta estado, conexión y conteo; no archiva ni elimina contactos.

## Código real

- [`../app/lib/tasks/contactdesk.rake`](../app/lib/tasks/contactdesk.rake)
- [`../app/app/services/contactdesk/diagnostics.rb`](../app/app/services/contactdesk/diagnostics.rb)

[EJECUTAR]

```bash
bin/rails contactdesk:diagnostics
```

## Errores comunes

- esconder reglas de negocio dentro de la tarea;
- imprimir nombres/emails en logs operativos;
- capturar cualquier excepción y devolver éxito;
- automatizar una reparación irreversible cuando sólo se pidió diagnosticar.

## Tu turno

Añade mentalmente una métrica `notes`. ¿En qué servicio debe calcularse para que HTTP y terminal puedan reutilizarla?

## Siguiente paso

La misma señal operativa puede exponerse de forma mínima a un monitor de disponibilidad.

## Referencias

- https://guides.rubyonrails.org/command_line.html#custom-rake-tasks
