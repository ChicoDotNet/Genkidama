# Checkpoint 04 — Reembolso frente a un receptor hostil

Trabaja sobre FreelanceEscrow sin abrir la solución.

## Escenario

El cliente puede ser un contrato y ejecutar código cuando recibe el reembolso. Tu objetivo es demostrar que el escrow no puede pagar dos veces durante una reentrada.

## Requisitos

1. Crea un contrato de prueba que sea el `client` de un nuevo FreelanceEscrow.
2. Durante su `receive()` intenta llamar `refund()` por segunda vez.
3. No hagas que el receptor revierta sólo para conseguir verde: registra si la segunda llamada tuvo éxito.
4. Añade una prueba que demuestre simultáneamente:
   - el escrow termina en `Refunded`;
   - el saldo del escrow termina en cero;
   - el cliente recibe exactamente el depósito una vez;
   - la segunda llamada a `refund()` falla.
5. No cambies producción si el comportamiento actual ya satisface la propiedad.

## Evidencia

Ejecuta:

```bash
forge test --match-path app/test/Security.t.sol -vvvv
bash tools/verify.sh
```

Explica qué línea conceptual de `refund()` impide el segundo pago y por qué este test no constituye una auditoría completa.

## Restricciones

- Usa sólo Ether ficticio de la EVM local.
- No despliegues a una red pública.
- No agregues un guard o dependencia sólo porque su nombre suene a seguridad.
- No debilites una aserción existente.

Cuando termines, compara tu enfoque con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).
