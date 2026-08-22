# Solución de referencia — Checkpoint 03

Una solución razonable conserva tres propiedades:

- el modo paralelo acepta un número acotado de workers;
- cada worker escribe sólo en su lote local;
- el merge final ordena resultados antes de exponerlos.

La implementación de referencia usa `std::jthread`, reparte subdirectorios por índice y deja que la destrucción de los `jthread` haga `join` antes de combinar lotes. Las pruebas comparan secuencial y paralelo elemento por elemento y verifican que `worker_count = 0` sea rechazado.

Para medir, usa `measure_discovery` y reporta los tiempos observados. No añadas una prueba que exija que el modo paralelo gane: el filesystem, la caché y la carga del runner pueden invertir el resultado.

La solución correcta no es la que usa más hilos, sino la que mantiene el contrato y puede justificar con evidencia cuándo la concurrencia aporta valor.
