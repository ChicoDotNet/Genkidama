# Lección 09 — Filtra por sensor sin copiar el archivo

## Qué vas a conseguir

Consultarás sólo las muestras de un sensor manteniendo memoria constante y conservando el orden original del archivo.

## El problema

`list` carga todo porque necesita entregar un arreglo al llamador. Para una consulta eso es innecesario: si sólo quieres el sensor 7, reservar memoria para miles de registros descartados desperdicia recursos.

## Concepto

C no trae iteradores de alto nivel integrados, pero una función callback permite procesar cada coincidencia mientras el parser avanza. `telemetry_query_file` recibe un `telemetry_filter`, un visitor y un contexto opaco propiedad del llamador.

El callback **no adquiere ownership** del registro: debe usarlo durante la llamada o copiarlo si necesita conservarlo.

[DEMO]

```bash
./app/build/telemetry_cli query sample.gtl 7 '*' '*'
```

`*` significa “sin ese filtro”. La API no interpreta strings; esa traducción pertenece a la CLI.

## Buenas prácticas

- valida `sensor_id != 0` antes de abrir el archivo;
- no escondas asignaciones dinámicas dentro de una consulta que puede ser streaming;
- conserva el orden del archivo salvo que el contrato prometa ordenar.

## Tu turno

Agrega tres muestras de dos sensores y comprueba que `query ... 7 * *` devuelve únicamente el sensor 7.

## Cómo comprobar tu solución

Ejecuta CTest y verifica también `Coincidencias: N` en la CLI.

## Siguiente paso

Continúa con [Lección 10 — Consulta intervalos temporales sin ambigüedad](10-consulta-intervalos-temporales.md).

## Referencias

- [cppreference: function pointers en C](https://en.cppreference.com/w/c/language/pointer)
