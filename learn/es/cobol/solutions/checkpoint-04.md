# Solución de referencia — Checkpoint 04

> Consulta esta referencia sólo después de intentar el checkpoint. No existe una única implementación correcta.

## Dirección de diseño

Una solución razonable crea acumuladores temporales para conteo y neto de bandas, recorre las cuatro posiciones con `PERFORM VARYING` y compara el resultado contra `WS-PROCESSED` y `WS-TOTAL-NET` antes de terminar el lote con éxito.

La comprobación debe vivir cerca de la finalización del proceso, no dentro de `ACCUMULATE-BAND`: queremos verificar de manera independiente el estado que ya fue construido.

## Contrato de resultado

Reserva un código de retorno nuevo, por ejemplo `6`, para una reconciliación imposible. Un diagnóstico breve puede distinguir conteos y netos.

No reutilices `2`, `3`, `4` o `5`, porque ya representan otras fronteras operativas.

## Prueba

El camino normal debe continuar pasando `bash tools/verify.sh` sin cambiar sus cifras.

Para probar el caso de reconciliación incorrecta sin romper la fuente canónica, una estrategia válida es copiar `src/nomina.cob` a un workspace temporal, introducir allí una alteración controlada antes de la verificación final, compilar esa copia y comprobar el código reservado. Otra estrategia reproducible es aceptable si conserva intacta la aplicación real.

## Criterio de revisión

Una respuesta fuerte demuestra que la reconciliación es una **invariancia**, no una cifra escrita a mano para el fixture actual. Cambiar el fixture válido debería cambiar totales y bandas juntos sin cambiar la lógica de la comprobación.

Vuelve a [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) y verifica que puedes explicar tanto el diseño como la evidencia ejecutable.
