# Checkpoint 01 — Constructor seguro

Trabaja sobre `FreelanceEscrow` después de completar las lecciones 1–4. No abras la solución antes de intentar el cambio.

## Escenario

El cliente intenta crear un escrow y, por un error de integración, proporciona `address(0)` como freelancer. El contrato actual puede aceptar esa dirección y dejar un pago futuro dirigido a una identidad inutilizable.

## Trabajo

Modifica el contrato para que el constructor rechace `address(0)` con un custom error específico llamado `InvalidFreelancer()`.

Agrega al menos una prueba que demuestre:

- un freelancer cero revierte con `InvalidFreelancer()`;
- un freelancer válido sigue permitiendo crear el escrow;
- el depósito y el resto de la suite existente permanecen verdes.

La validación debe vivir en el contrato, no sólo en una UI o script de despliegue.

## Restricciones

- No elimines la validación `EmptyDeposit()`.
- No cambies las transiciones `Funded -> Delivered -> Released` ni `Funded -> Refunded`.
- No uses una red pública ni fondos reales.
- No relajes las pruebas existentes para obtener verde.

## Comprobación

Desde la raíz del curso:

```bash
bash tools/verify.sh
```

Después ejecuta específicamente tu prueba nueva con `forge test --match-test <nombre> -vv`.

## Reflexión

En dos o tres frases explica por qué validar una dirección crítica en el contrato es diferente de validarla únicamente antes del despliegue.

Cuando termines, compara con [`../solutions/checkpoint-01.md`](../solutions/checkpoint-01.md).
