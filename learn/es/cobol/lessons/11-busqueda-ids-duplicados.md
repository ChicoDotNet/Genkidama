# Lección 11 — Buscar IDs y proteger la integridad del lote

## Qué vas a conseguir

Evitarás que el mismo ID de empleado aceptado se procese dos veces dentro de una ejecución y protegerás los totales contra duplicados.

## Antes de empezar

Completa la [Lección 10](10-perform-varying-y-reportes.md).

## El problema

Un archivo puede repetir por accidente una fila válida. Si el batch suma ambas ocurrencias, el reporte parece consistente matemáticamente pero representa dos veces al mismo empleado. Ese defecto es más peligroso que un error de formato porque puede pasar desapercibido.

## Concepto

NominaBatch conserva hasta 100 IDs aceptados en una tabla `OCCURS`. Antes de calcular un nuevo registro recorre sólo las posiciones ya ocupadas. Si encuentra el ID actual, lo rechaza y sale del párrafo antes de modificar importes o agregados.

```text
PERFORM UNTIL WS-SEEN-POS > WS-SEEN-COUNT
    IF WS-SEEN-ID(WS-SEEN-POS) = FUNCTION TRIM(WS-ID-TEXT)
        MOVE "Y" TO WS-DUPLICATE-FOUND
        EXIT PERFORM
    END-IF
    ADD 1 TO WS-SEEN-POS
END-PERFORM
```

Ésta es una búsqueda lineal deliberada. Con un límite pedagógico de 100 registros es fácil de leer y suficiente. No fingimos que sea la solución para millones de filas.

## Demostración

[EN PANTALLA] El fixture incluye una segunda fila `E001`. La primera se acepta; la segunda debe producir:

```text
RECHAZADO|E001|ID duplicado en el lote
```

Después compara el resumen: el duplicado no incrementa `PROCESADOS`, bruto, deducciones, neto ni bandas.

## Código real

La secuencia importa:

1. validar campos y rangos;
2. buscar duplicado;
3. verificar capacidad de la tabla;
4. calcular;
5. registrar el ID como aceptado;
6. actualizar totales y bandas.

Esto evita guardar como "visto" un registro inválido que nunca fue procesado.

## Qué acaba de pasar

Introdujimos una regla de integridad del lote sin mezclarla con lectura de archivos. La tabla es estado de una ejecución, no persistencia entre ejecuciones.

## Errores comunes

- registrar el ID antes de terminar validación;
- sumar importes antes de comprobar duplicado;
- recorrer siempre las 100 posiciones cuando sólo hay tres ocupadas;
- tratar un límite fijo como si fuera escalabilidad ilimitada;
- aceptar silenciosamente el segundo registro y sobrescribir expectativas;
- deduplicar por nombre cuando el contrato dice ID.

## Buenas prácticas

La política ante duplicados debe ser explícita. Aquí rechazamos la segunda aparición porque es determinista, observable y conserva la primera transacción válida. Para otro negocio podría ser correcto rechazar el lote completo; eso sería una decisión distinta que debería documentarse y probarse.

## Tu turno

Mueve temporalmente la fila duplicada antes de `E001` y explica cuál ocurrencia debería aceptarse bajo esta política. Después restaura el fixture canónico.

## Cómo comprobar

```text
bash tests/smoke.sh
```

El smoke comprueba tanto el rechazo como los totales exactos, de modo que una deduplicación que ocurra demasiado tarde también falla.

## Solución enlazada

No hay solución independiente para este ejercicio breve.

## Reto adicional

Describe dos alternativas para superar el límite de 100 IDs: una estructura distinta dentro del programa y una validación previa fuera de este proceso. Compara complejidad y memoria sin implementarlas.

## Resumen

Una búsqueda pequeña puede proteger una regla importante si se coloca antes de los efectos contables. El objetivo no es presumir algoritmos, sino preservar integridad.

## Siguiente paso

Continúa con la [Lección 12 — Límites, reconciliación y checkpoint 03](12-limites-reconciliacion-y-checkpoint.md).

## Referencias

- [GnuCOBOL Manual](https://gnucobol.sourceforge.io/doc/gnucobol.html)
