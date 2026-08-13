# Solución de referencia — Evaluación final COBOL

> Abre esta referencia sólo después de completar un intento. No existe una única solución correcta.

## Dirección de diseño

Una solución razonable normaliza identidad y valida el bono antes de cualquier acumulador. El reporte y las tablas consumen únicamente registros completamente aceptados. Para compatibilidad, ausencia del campo de bono en un registro histórico se interpreta como cero; un campo presente pero inválido se rechaza explícitamente.

## Bono opcional

El orden conceptual es: separar campos → detectar variante legacy/nueva → validar bono si existe → convertir → calcular bruto base + bono → calcular deducción → actualizar totales, bandas e IDs. Conserva al menos una entrada legacy en las pruebas para demostrar compatibilidad.

## Identidad

La referencia usa una representación canónica sin espacios exteriores. La normalización ocurre antes de comprobar vacío y antes de buscar duplicados. Así `E001`, `E001 ` y ` E001` representan la misma identidad lógica. La regresión debe comprobar tanto el rechazo como que los totales permanecen intactos.

## Regresión

Ejecuta:

```bash
bash tools/verify.sh
```

No elimines escenarios operativos ni cambies códigos de retorno para acomodar la nueva funcionalidad.

## Documentación

Consulta documentación oficial para justificar manipulación alfanumérica, conversión numérica y la `PICTURE` elegida. La evidencia importante es conectar cada fuente con una decisión concreta.

## Diseño esperado

La normalización pertenece a una frontera determinista anterior a las reglas dependientes del ID. El bono debe preservar centavos y detectar overflow. Un formato futuro debe evolucionar mediante variantes/versiones explícitas, no mediante inferencias ambiguas. Antes de reemplazar la búsqueda lineal acotada se mide tamaño real del lote y costo de búsqueda. Si cambia el origen de entrada, se sustituye primero el adaptador de I/O; cálculo, validación y reconciliación no deberían depender del dispositivo.

## Defensa de entrevista

Distingue contrato de registro, validación, reglas monetarias, acumulación e I/O. Reconoce límites: GnuCOBOL local no demuestra JCL, CICS, DB2 ni operación z/OS; sí demuestra razonamiento sobre un batch COBOL pequeño, reproducible y probado.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y evalúa comportamiento y explicación, no similitud de líneas.
