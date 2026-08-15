# Solución de referencia — Checkpoint 03

Una solución razonable agrega `WS-BAND-GROSS PIC 9(10)V99 VALUE ZERO` a cada elemento de la tabla y acumula `WS-GROSS` en la banda elegida sólo después de que el registro fue aceptado.

No muevas la acumulación antes de validación o detección de duplicados: un rechazo no debe contaminar agregados. Para reportar, usa un `PIC` de display separado y conserva `PERFORM VARYING`; evita cuatro bloques duplicados.

La prueba debe verificar los cuatro brutos por banda y el total global. Lo importante es demostrar tres contratos: cada aceptado pertenece a una sola banda, rechazados no tienen efectos contables y la suma del bruto por bandas reconcilia con el resumen global.
