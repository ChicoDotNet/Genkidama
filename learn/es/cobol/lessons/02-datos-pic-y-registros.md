# Lección 2 — Datos `PIC` y registros

## Qué vas a conseguir
Entenderás cómo COBOL describe datos con `PIC`, grupos y decimales implícitos, y cómo NominaBatch separa texto de entrada de valores numéricos de negocio.

## Antes de empezar
Ejecuta de nuevo NominaBatch y abre `WORKING-STORAGE SECTION` en [`../app/src/nomina.cob`](../app/src/nomina.cob).

## El problema
Un archivo llega como texto, pero una regla de nómina necesita números con capacidad y escala conocidas. Tratar todo como cadenas oculta errores; convertir demasiado pronto vuelve frágil el parsing.

## Concepto
`PIC X(40)` reserva caracteres. `PIC 9(3)` representa tres dígitos. En `PIC 9(5)V99`, la `V` marca dos decimales implícitos: no ocupa un carácter, pero define la escala numérica.

Los niveles `01` y `05` permiten agrupar campos relacionados. NominaBatch usa un grupo para texto parseado y otro para valores numéricos ya validados.

## Demostración
[EN PANTALLA] Compara:

```text
05 WS-RATE-TEXT       PIC X(16).
05 WS-HOURLY-RATE     PIC 9(5)V99.
```

El primero conserva la entrada; el segundo sólo debe recibir un número aceptado.

## Código real
`UNSTRING` divide cada línea usando `;`. El resultado entra primero en campos `X`. Más tarde, después de validar, `FUNCTION NUMVAL` convierte horas, tarifa y porcentaje.

## Qué acaba de pasar
La estructura de datos hace visible una frontera: **texto no confiable antes de validación; número de negocio después**.

## Errores comunes
- pensar que `V` es un punto decimal almacenado;
- elegir una `PICTURE` demasiado pequeña y perder rango;
- usar el mismo campo para entrada cruda y dato ya validado;
- cambiar el formato del fixture sin ajustar el contrato de parsing.

## Buenas prácticas
Nombra por significado, dimensiona con límites de negocio explícitos y conserva la entrada externa separada de los valores usados para calcular.

## Tu turno
Calcula cuál es el máximo valor representable por `PIC 9(5)V99` y comprueba si cubre cómodamente las tarifas del fixture. No cambies la `PICTURE` si no existe una necesidad real.

## Cómo comprobar
Debes poder explicar por qué `250.00` cabe en `9(5)V99` y por qué `WS-RATE-TEXT` sigue siendo alfanumérico.

## Solución enlazada
No necesitas una solución de referencia: la respuesta se deriva de la `PICTURE` y del fixture.

## Reto adicional
¿Qué pasaría si una futura tarifa pudiera ser negativa? Describe qué decisión de dominio y representación tendrías que tomar antes de editar código.

## Resumen
- `PIC` expresa forma y capacidad;
- `V` define escala decimal implícita;
- grupos documentan estructura;
- texto externo y número validado son responsabilidades distintas.

## Siguiente paso
Continúa con [Lección 3 — Validación y aritmética decimal](03-validacion-y-calculo-nomina.md).

## Referencias
- [GnuCOBOL Programmer's Guide](https://gnucobol.sourceforge.io/HTML/gnucobpg.html)
