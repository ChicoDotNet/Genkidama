# Checkpoint 01 — Un cliente, varias horas y un total verificable

## Objetivo

Recuperar lo aprendido en las primeras cuatro lecciones sin copiar una receta completa.

## Misión

Modifica TimeQuote para demostrar este escenario:

1. crea un cliente con tarifa válida;
2. registra al menos tres entradas de tiempo;
3. una entrada debe tener `note: nil`;
4. calcula el total acumulado;
5. añade una prueba para una regla inválida que todavía no esté cubierta por tus cambios.

[PAUSA PARA EJERCICIO]

Antes de mirar una referencia, intenta resolverlo sólo con el código existente, las pruebas actuales y la documentación oficial enlazada en las lecciones.

## Evidencia

Ejecuta:

```bash
swift test
swift run TimeQuote
```

Conserva ambas salidas. Debes poder explicar por qué usaste `let` o `var`, dónde aparece un optional y qué error protege tu nueva prueba.

## Reto adicional

Imprime `allSummaries()` para dos clientes y verifica que el orden sea alfabético.

## Solución

Cuando hayas terminado tu intento, compara decisiones con [la solución de referencia](../solutions/checkpoint-01.md).

## Siguiente paso

Continúa con [la lección 05 — Protocolos como contratos reemplazables](05-protocolos-y-contratos.md).
