# Lección 2 — Datos de negocio: strings, fechas, Decimal y dataclasses

## Qué vas a conseguir

Vas a leer cómo LedgerMatch transforma texto del CSV en datos con significado: identificadores, nombres, fechas y cantidades monetarias.

## Antes de empezar

Ejecuta el ejemplo de la lección 1 y abre `src/ledgermatch/models.py`.

## El problema

Un CSV sólo contiene texto. Pero `"2026-08-01"`, `"1250.00"` y `"Acme Norte"` no significan lo mismo. Si todo permanece como texto, las reglas de negocio terminan llenas de conversiones y errores difíciles de localizar.

## Concepto

Python tiene tipos incorporados como `str`, `int` y `bool`, y una biblioteca estándar amplia. LedgerMatch usa además:

- `date` para fechas;
- `Decimal` para dinero;
- `dataclass` para agrupar datos relacionados;
- `StrEnum` para estados con nombres controlados.

Un *type hint* como `invoice_id: str` documenta la intención y ayuda a herramientas; no convierte automáticamente datos externos en datos válidos.

## Demostración

Prueba en el intérprete:

```python
from decimal import Decimal

print(0.1 + 0.2)
print(Decimal("0.1") + Decimal("0.2"))
```

[EN PANTALLA] Compara las dos representaciones. `float` es excelente para muchos cálculos, pero una conciliación monetaria necesita una decisión más explícita.

## Código real

En `models.py` encontrarás:

```python
@dataclass(frozen=True, slots=True)
class InvoiceRecord:
    invoice_id: str
    customer: str
    issued_on: date
    invoice_total: Decimal
    payment_total: Decimal
```

`frozen=True` evita cambios accidentales después de construir el registro. `slots=True` hace explícito el conjunto de atributos y reduce parte del overhead de cada instancia.

En `parser.py`, `Decimal(text)` convierte el texto sólo después de validar que existe.

## Qué acaba de pasar

La frontera CSV recibe strings. El parser transforma una fila válida en `InvoiceRecord`. A partir de ahí, la conciliación trabaja con datos tipados y no necesita saber cómo venían escritos en el archivo.

Esa separación parece pequeña, pero es arquitectura: la regla que compara importes no debería ocuparse de abrir archivos.

## Errores comunes

- Usar `float` por costumbre para dinero.
- Pensar que un type hint valida automáticamente JSON/CSV/teclado.
- Crear diccionarios anónimos para todo y perder significado.
- Convertir datos sin conservar contexto suficiente para explicar errores.

## Buenas prácticas

Elige tipos que comuniquen intención. No construyas clases para cada valor, pero tampoco dejes que todo sea `str` porque “así llega del CSV”.

## Tu turno

Abre `reconciler.py` y localiza `difference=record.payment_total - record.invoice_total`.

Predice el signo de la diferencia para:

- factura 100, pago 90;
- factura 100, pago 110.

Después modifica temporalmente el CSV para comprobarlo.

## Cómo comprobar

Un pago menor produce `-10.00`; un pago mayor produce `+10.00`. El signo conserva información útil: no sólo sabemos que algo difiere, también en qué dirección.

## Solución

No necesitas `abs(...)`. LedgerMatch conserva el signo deliberadamente porque más adelante un reporte podrá distinguir faltantes de pagos superiores al importe facturado.

## Reto adicional

Añade en el intérprete dos `Decimal` construidos desde strings con centavos y comprueba que la suma es exacta. Luego construye uno desde un `float` y observa por qué la documentación recomienda partir de una representación decimal confiable.

## Resumen

- CSV entra como texto.
- Los tipos comunican significado y reducen conversiones dispersas.
- `Decimal` es una elección deliberada para esta aritmética monetaria.
- `dataclass` permite modelar registros sin escribir boilerplate innecesario.

## Siguiente paso

En la [Lección 3](03-validacion-y-errores.md) verás qué ocurre cuando el mundo real entrega celdas vacías, fechas imposibles o importes que no son números.

## Referencias

- [`decimal`](https://docs.python.org/3.14/library/decimal.html)
- [`dataclasses`](https://docs.python.org/3.14/library/dataclasses.html)
- [`datetime`](https://docs.python.org/3.14/library/datetime.html)
