# Checkpoint 01 — Duración segura sin romper la agenda

AgendaPHP ya rechaza duraciones menores a 15 o mayores a 480 minutos. Un negocio pide una regla adicional para esta primera versión:

> Las citas capturadas desde la web sólo pueden elegir bloques de 30, 45, 60, 90 o 120 minutos.

## Tu encargo

Sin mover la regla de traslape al HTML:

1. protege con una prueba el comportamiento esperado para las duraciones ofrecidas por la web;
2. demuestra que una duración web de `75` se rechaza antes de persistir;
3. conserva `Appointment` como modelo capaz de representar otros intervalos válidos de 15–480 minutos, porque futuras integraciones podrían necesitarlos;
4. muestra un error recuperable al usuario sin perder cliente, servicio e inicio;
5. mantén `bash tools/verify.sh` verde.

## Criterio de éxito

La restricción de opciones de captura web no debe convertirse accidentalmente en una regla universal del objeto `Appointment`.

Explica en dos o tres frases **por qué** colocaste la nueva validación en la frontera elegida.

Cuando termines, compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).
