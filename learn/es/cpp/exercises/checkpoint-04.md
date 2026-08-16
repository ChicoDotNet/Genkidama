# Checkpoint 04 — Operación robusta

## Encargo

Extiende ThreadSeek sin cambiar su contrato determinista:

1. acepta un `std::stop_token` desde una fuente externa;
2. reporta progreso sin exponer el vector interno de resultados;
3. provoca que una entrada desaparezca durante el recorrido y conserva el resto del índice;
4. compara secuencial y paralelo sin imponer cuál debe ganar;
5. ejecuta la matriz GCC/Clang/MSVC sin relajar warnings.

## Evidencia esperada

- test de cancelación cooperativa;
- test de mutación del filesystem;
- comparación con resultados equivalentes;
- explicación breve de por qué los tiempos no son un gate de CI;
- build y CTest verdes en la matriz soportada.

## Restricciones

No mates threads, no agregues un mutex global para el vector principal, no captures excepciones indiscriminadamente y no reduzcas los niveles de warning para conseguir verde.

Cuando termines, compara tu solución con la [referencia](../solutions/checkpoint-04.md).
