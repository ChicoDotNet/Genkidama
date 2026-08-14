# Solución de referencia — Checkpoint 01

> Consulta esta referencia sólo después de completar un intento.

Una dirección razonable es ampliar `CreateQuoteInput` con `discountPercent?: number` y `Quote` con `discountPercent` y `total`, preservando `subtotal`.

La validación pertenece a la regla de cotización, no al HTTP:

```ts
const discountPercent = input.discountPercent ?? 0;
if (!Number.isFinite(discountPercent) || discountPercent < 0 || discountPercent > 100) {
  throw new Error("El descuento debe estar entre 0 y 100.");
}
const total = subtotal * (1 - discountPercent / 100);
```

Pruebas útiles:

- sin campo opcional: `subtotal === total`;
- 10% sobre 1000: subtotal 1000, total 900;
- -1 y 101: error;
- API: una petición válida conserva el descuento en la respuesta.

No conviertas el descuento en lógica del formulario. El navegador sólo captura y presenta datos; el dominio define el significado.
