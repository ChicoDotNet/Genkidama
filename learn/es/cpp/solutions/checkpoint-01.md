# Solución de referencia — Checkpoint 01

No hay una única solución correcta. La referencia conserva el índice como datos puros y extiende la consulta en lugar de volver a tocar disco.

## Contrato sugerido

Puedes introducir un `SearchOptions` pequeño:

```cpp
struct SearchOptions {
    std::string_view text;
    std::string_view extension;
};
```

Después cambia `search` para recibir esas opciones. Normaliza la extensión una vez antes del loop: si no empieza por `.`, agrégalo; si queda vacía, no filtres.

## Regla central

Dentro del recorrido de `files_`, primero verifica el texto y luego compara `record.path.extension().string()` con la extensión normalizada. Ambas comparaciones deben compartir la misma normalización ASCII para evitar dos contratos distintos.

## Pruebas de referencia

Usa el fixture existente: `.txt` debe devolver `Manual.TXT` y `notes.txt`; `md` debe devolver `README.md`; `.csv` debe devolver cero elementos.

## Qué evitar

No añadas la extensión como propiedad permanente a `FileRecord` sólo para esta consulta: ya puede derivarse del path. No hagas un segundo `recursive_directory_iterator`. No muevas la búsqueda al CLI.

## Siguiente incremento

Una vez que tu versión esté verde, vuelve al README. Las lecciones 5–8 introducirán fronteras de descubrimiento/persistencia antes de medir y paralelizar.
