# Checkpoint 02 — Índice durable

Sin copiar la solución, extiende ThreadSeek para demostrar estas propiedades:

1. descubrimiento de filesystem separado del índice consultable;
2. un índice puede reconstruirse desde registros en memoria;
3. el índice puede guardarse y cargarse sin volver a escanear la raíz;
4. una escritura incompleta no debe reemplazar silenciosamente un índice válido;
5. un formato corrupto debe producir un error explícito;
6. las pruebas existentes de búsqueda deben seguir pasando.

## Evidencia

Entrega el código, el resultado de CMake/CTest y una explicación breve de qué objetos son dueños de archivos, vectores y streams. No agregues `new/delete` si los tipos estándar ya expresan correctamente el ownership.

Compara después con la [solución de referencia](../solutions/checkpoint-02.md).
