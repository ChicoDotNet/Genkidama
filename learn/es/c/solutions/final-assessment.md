# Solución de referencia — Evaluación final

Esta referencia no pretende ser la única solución correcta. Úsala después de intentar la evaluación para comparar decisiones y contratos.

## Enfoque de referencia

Una extensión prudente es agregar una consulta `last` que obtenga el último registro de un sensor reutilizando `telemetry_query_file`.

La implementación puede mantener un pequeño contexto de callback con:

- `sensor_id` objetivo;
- indicador `found`;
- último `telemetry_record` observado.

Cada coincidencia reemplaza el valor anterior. Como el parser conserva el orden físico del archivo, al terminar tienes la última coincidencia sin reservar un vector proporcional al número de registros.

## Contrato sugerido

Si expones una función pública, documenta algo equivalente a:

- entrada: ruta, sensor y puntero de salida;
- retorno: `TELEMETRY_OK` si encontró registro;
- error explícito si la ruta/archivo es inválido;
- estado distinto para “archivo válido pero sensor sin registros” si tu API ya dispone de uno, o un booleano/resultado documentado que no confunda ausencia con corrupción;
- ningún ownership dinámico nuevo si escribes directamente en el `telemetry_record` del llamador.

## Bugfix de referencia

Un buen caso límite es comprobar que una consulta con `start == end` no lea registros y que `start > end` sea rechazada por contrato. La prueba debe cubrir exactamente el límite para impedir una regresión hacia intervalos cerrados ambiguos.

Otra solución válida es reforzar un failure mode de recovery siempre que primero exista una prueba que demuestre el defecto.

## Failure mode

La nueva capacidad no debe ocultar errores del parser. Si el archivo termina truncado después de registros válidos, `last` no debería devolver silenciosamente el último registro como si el stream completo fuera confiable: debe propagar el defecto del archivo.

Ese detalle diferencia “encontré una coincidencia” de “validé correctamente la consulta completa”.

## Prueba de referencia

Construye un fixture con al menos:

1. dos registros del sensor objetivo;
2. un registro de otro sensor entre ambos;
3. verificación de que `last` devuelve la segunda coincidencia;
4. un caso sin coincidencias;
5. si aplica, un archivo con sufijo truncado que demuestre propagación de error.

La prueba debe observar el contrato público, no variables internas del parser.

## Documentación oficial

Una decisión posible es confirmar en la documentación de CMake que `C_STANDARD 23` expresa el dialecto solicitado en el target. Otra es consultar la documentación del compilador antes de asumir soporte de una característica C23.

La evidencia importante no es pegar una URL: es explicar qué supuesto técnico verificaste antes de decidir.

## Mejora futura razonable

Una siguiente evolución podría ser un índice auxiliar por sensor/timestamp para acelerar consultas muy grandes.

Trade-offs:

- mejora potencial de latencia;
- añade otro artefacto persistido que debe mantenerse consistente;
- introduce versionado, invalidación y recuperación adicionales;
- exige benchmarks con datasets representativos antes de justificar complejidad.

Por eso no lo introduciríamos sólo porque “un índice suena más profesional”. Primero mediríamos el cuello de botella real.

## Qué comparar con tu solución

Pregunta:

- ¿preservaste el formato existente?
- ¿mantuviste ownership explícito?
- ¿propagaste corrupción en vez de esconderla?
- ¿tu prueba fallaría si el bug regresa?
- ¿tu nueva capacidad escala sin cargar el archivo completo?
- ¿puedes explicar por qué tu alternativa es mejor para el requisito que elegiste?

Si tu diseño responde bien a esas preguntas, puede ser tan válido como esta referencia.