# Lección 16 — Seguridad de entradas y hardening

## Qué vas a conseguir

Vas a endurecer fronteras de LedgerMatch sin confundir controles concretos con una certificación de seguridad.

## El problema

Una herramienta local puede destruir información o consumir recursos accidentalmente: archivos enormes, carpetas usadas como CSV, sobrescrituras, salida sobre la entrada o texto interpretado como fórmula por hojas de cálculo.

## Controles

1. `validate_input_file()` exige archivo regular y aplica un default de 10 MiB configurable por `--max-input-bytes` / `LEDGERMATCH_MAX_INPUT_BYTES`.
2. reportes usan creación exclusiva por default; `--force` autoriza reemplazar sólo reportes existentes.
3. `validate_report_destinations()` corre antes de persistir y nunca permite que JSON/CSV coincidan con la entrada ni entre sí.
4. CSV neutraliza prefijos de fórmula únicamente en texto no confiable (`invoice_id`, `customer`), no en números negativos legítimos.
5. logs no copian filas completas.

Estos controles no reemplazan permisos del SO, backups, cifrado, identidad/autorización ni operación multiusuario.

## Tu turno — Checkpoint 04

Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin mirar la solución.

## Cómo comprobar

```bash
python -m pytest
ledgermatch examples/invoices.csv --json examples/invoices.csv --force
```

La segunda operación debe rechazarse sin modificar la entrada.

## Solución

Después compara con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).

## Errores comunes

- tratar `--force` como “sin reglas”;
- sanitizar números negativos como si fueran texto;
- capturar I/O y devolver éxito;
- confiar sólo en extensión `.csv`.

## Buenas prácticas

- valida antes de efectos irreversibles;
- limita recursos según contexto;
- protege siempre la fuente;
- aplica controles según el tipo de dato;
- documenta lo que queda fuera de alcance.

## Reflexión

¿Por qué detectar colisiones antes de persistir deja un comportamiento más limpio que descubrirlas después de modificar SQLite?

## Resumen

- fronteras de archivo necesitan política;
- límites reducen consumo accidental;
- sobrescribir requiere intención;
- la fuente nunca es destino;
- hardening parte de amenazas concretas.

## Siguiente paso

En la [Lección 17](17-evaluacion-final.md) modificarás LedgerMatch desde requisitos, pruebas y documentación oficial sin receta guiada.

## Referencias

- [`pathlib`](https://docs.python.org/3.14/library/pathlib.html)
- [`csv`](https://docs.python.org/3.14/library/csv.html)
