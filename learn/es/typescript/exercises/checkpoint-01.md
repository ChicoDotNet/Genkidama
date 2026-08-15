# Checkpoint 01 — Descuento sin romper contratos

Trabaja sobre FreelanceDesk después de la Lección 04. No abras la solución antes de completar un intento.

## Historia

Algunos clientes reciben un descuento porcentual opcional por cotización.

Implementa el cambio con estas reglas:

- una cotización sin descuento conserva exactamente el comportamiento actual;
- el descuento permitido está entre 0 y 100 inclusive;
- el subtotal original sigue disponible;
- agrega un total final después del descuento;
- un descuento inválido produce un error útil;
- el servidor acepta el campo opcional en `POST /api/quotes`;
- agrega al menos dos pruebas: una válida y una inválida.

No se prescribe el nombre exacto de cada helper. Evita llevar la regla al DOM o al manejador HTTP.

## Evidencia mínima

```bash
npm run check
npm test
```

Después crea una cotización manual con 10% de descuento y explica qué parte fue comprobada por TypeScript y qué parte requirió validación runtime.
