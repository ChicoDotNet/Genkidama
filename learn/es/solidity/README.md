# Curso de Solidity desde cero — Construye FreelanceEscrow

Curso práctico en español para aprender Solidity construyendo **FreelanceEscrow**, un contrato de cobro para proyectos freelance. El proyecto conserva cuatro capacidades centrales durante todo el curso: depósito del cliente, registro de entrega, liberación del pago y reembolso.

Solidity se usa principalmente para contratos inteligentes compatibles con la EVM. Es un mercado especializado y con responsabilidades de seguridad altas; este curso prepara fundamentos y evidencia práctica, pero **no promete empleo** ni presenta Web3 como un mercado junior masivo.

## ¿Puedo aprenderlo desde cero?

Sí. No necesitas conocer Solidity ni desarrollo blockchain previamente. Conviene sentirte cómodo usando una terminal y un editor. El curso explica los conceptos de EVM necesarios cuando aparecen y no enseña especulación, trading ni lanzamiento de tokens.

## Qué vas a construir

`FreelanceEscrow` modela un acuerdo simple entre dos participantes:

```text
Cliente deposita -> Freelancer entrega -> Cliente libera -> Freelancer cobra
       |
       +-------------------------------> Reembolso antes de entrega
```

La misma aplicación crece para enseñar tipos, estado, funciones, visibilidad, errores, eventos, Ether, ABI, pruebas, tooling, diagnóstico, gas, seguridad y diseño de contratos.

## Estado del curso

**Completo: 17/17 lecciones.** El paquete final incluye cuatro checkpoints, evaluación autónoma sin receta, rúbrica de 100 puntos, solución de referencia, narrativa de entrevista y un gate CI que exige el paquete final, formato, build y suite Forge completa. El SHA funcional `023b6a95b98e7d0e16eec326175b4b1f7a436c35` pasó **Learn Solidity** y **Genkidama Learn**.

## Qué necesitas instalar

Usamos una release estable de [Foundry](https://getfoundry.sh/introduction/installation/) y `solc 0.8.35` fijado por [`foundry.toml`](foundry.toml).

Comprueba Foundry:

```bash
forge --version
```

No necesitas una wallet, una RPC comercial ni Ether real para completar estas lecciones.

## Build, test y verificación

Desde la raíz de este curso:

```bash
forge build
forge test -vv
bash tools/verify.sh
```

`tools/verify.sh` es el gate local: comprueba formato, compilación y pruebas usando la misma configuración de proyecto que CI.

Para inspeccionar o diagnosticar:

```bash
forge inspect FreelanceEscrow abi
forge test --match-test testCannotRefundAfterDelivery -vvvv
forge test --gas-report
forge build --sizes
```

## Lecciones

1. [Primer depósito y estado del escrow](lessons/01-primer-deposito-y-estado.md)
2. [Máquina de estados, roles y entrega](lessons/02-maquina-de-estados-roles-y-entrega.md)
3. [Liberación, reembolso y transferencia de valor](lessons/03-liberacion-reembolso-y-transferencia-de-valor.md)
4. [Reverts precisos, suite y checkpoint](lessons/04-reverts-precisos-suite-y-checkpoint.md)
5. [ABI: el contrato que otros programas realmente ven](lessons/05-abi-la-frontera-publica.md)
6. [Eventos como contrato observable](lessons/06-eventos-como-contrato-observable.md)
7. [Interfaces explícitas y compatibilidad](lessons/07-interfaces-explicitas-y-compatibilidad.md)
8. [Transferencias, atomicidad y checkpoint 02](lessons/08-transferencias-atomicidad-y-checkpoint.md)
9. [Fuzzing: propiedades sobre valor](lessons/09-fuzzing-propiedades-de-valor.md)
10. [Invariantes de estado y autorización](lessons/10-invariantes-de-estado-y-autorizacion.md)
11. [Composición y colaboradores hostiles](lessons/11-composicion-y-colaboradores-hostiles.md)
12. [Estrategia de pruebas y checkpoint 03](lessons/12-estrategia-de-pruebas-y-checkpoint-03.md)
13. [Tooling y superficie profesional](lessons/13-tooling-y-superficie-profesional.md)
14. [Diagnóstico con traces](lessons/14-diagnostico-con-traces.md)
15. [Gas y rendimiento con evidencia](lessons/15-gas-y-rendimiento-con-evidencia.md)
16. [Hardening, reentrada y checkpoint 04](lessons/16-hardening-reentrada-y-checkpoint-04.md)
17. [Evaluación final sin receta](lessons/17-evaluacion-final.md)

### Checkpoint 01
- [Ejercicio — Constructor seguro](exercises/checkpoint-01.md)
- [Solución de referencia](solutions/checkpoint-01.md) — ábrela sólo después de tu intento.

### Checkpoint 02
- [Ejercicio — Falla de pago sin corromper el escrow](exercises/checkpoint-02.md)
- [Solución de referencia](solutions/checkpoint-02.md) — ábrela sólo después de tu intento.

### Checkpoint 03
- [Ejercicio — Propiedades de reembolso](exercises/checkpoint-03.md)
- [Solución de referencia](solutions/checkpoint-03.md) — ábrela sólo después de tu intento.

### Checkpoint 04
- [Ejercicio — Reembolso frente a un receptor hostil](exercises/checkpoint-04.md)
- [Solución de referencia](solutions/checkpoint-04.md) — ábrela sólo después de tu intento.

### Evaluación final
- [Evaluación — Evoluciona FreelanceEscrow sin receta](exercises/evaluacion-final.md)
- [Rúbrica final — 100 puntos](exercises/rubrica-final.md)
- [Solución de referencia](solutions/evaluacion-final.md) — sólo después de tu intento.

## Qué sabrás hacer al terminar

La meta 0 → Junior es que puedas leer un contrato sencillo, implementar y probar cambios sin una receta línea por línea, razonar sobre estado y autorización, manejar Ether y errores, usar Foundry, consultar documentación oficial, reconocer riesgos comunes, diagnosticar un revert y explicar las decisiones de FreelanceEscrow en una entrevista.

## Seguridad

Todos los ejemplos usan ejecución local y valores ficticios. Un ejercicio educativo verde no convierte un contrato en software listo para custodiar dinero real. Un despliegue que maneje valor real requiere threat modeling, revisión especializada, pruebas adicionales y, según el riesgo, auditoría independiente.

## Preguntas frecuentes

### ¿Necesito MetaMask?
No para el curso base. Foundry proporciona cuentas y una EVM local para pruebas.

### ¿Vamos a crear un token?
No. El proyecto se mantiene enfocado en custodia y liquidación de un pago freelance.

### ¿Foundry es parte de Solidity?
No. Solidity es el lenguaje y Foundry es el toolchain elegido para compilar y probar localmente.

### ¿La ABI es lo mismo que el contrato?
No. La ABI describe la frontera codificable que consumen herramientas e integraciones; el bytecode y la lógica interna son artefactos distintos.

### ¿Qué aporta fuzzing si ya tengo pruebas normales?
Permite expresar una propiedad y comprobarla sobre muchas entradas generadas, mientras las pruebas deterministas siguen documentando historias concretas.

### ¿Una prueba de reentrada demuestra que el contrato es seguro?
No. Protege una propiedad concreta frente a un receptor hostil. No sustituye threat modeling ni auditoría.

### ¿Este proyecto está listo para producción?
No. Es una aplicación educativa; pruebas verdes no equivalen a auditoría de seguridad.

## Glosario inicial

- **EVM:** máquina virtual que ejecuta bytecode de contratos compatibles.
- **Wei/Ether:** unidades de valor nativo; `1 ether` es una unidad de conveniencia.
- **`msg.sender`:** dirección que realiza la llamada actual.
- **`msg.value`:** valor enviado junto con una llamada `payable`.
- **ABI:** convención para codificar llamadas, argumentos, resultados, eventos y errores.
- **selector:** primeros cuatro bytes derivados de la firma canónica de una función o error.
- **event/log:** señal observable emitida durante una ejecución exitosa.
- **custom error:** error tipado que puede devolver datos al llamador.
- **atomicidad:** una transacción confirma todos sus efectos o, si revierte, no confirma un estado parcial.
- **fuzzing:** ejecución repetida de una propiedad con entradas generadas por el framework de pruebas.
- **invariante:** afirmación que debe conservarse para una región o secuencia válida del sistema.
- **trace:** secuencia detallada de llamadas y efectos usada para diagnosticar una ejecución.
- **reentrada:** nueva entrada a un contrato durante una interacción externa antes de que termine la llamada original.

## Cómo hablar de este proyecto en una entrevista

Explica FreelanceEscrow como una máquina de estados pequeña: el cliente deposita, el freelancer declara entrega y el cliente libera o reembolsa antes de la entrega. Describe cómo custom errors, eventos y roles inmutables hacen explícitos los contratos; cómo Foundry protege historias, fuzzing y colaboradores hostiles; y cómo `checks → effects → interactions` protege el reembolso probado contra una segunda entrada. Cierra declarando límites: pruebas locales no equivalen a auditoría y el proyecto no implementa disputas, upgrades ni oráculos.

## Referencias oficiales

- [Solidity documentation](https://docs.soliditylang.org/en/v0.8.35/)
- [Solidity contracts](https://docs.soliditylang.org/en/v0.8.35/contracts.html)
- [Solidity ABI specification](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html)
- [Solidity security considerations](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html)
- [Foundry Book](https://getfoundry.sh/)
- [Foundry — Writing Tests](https://getfoundry.sh/forge/writing-tests)
- [Foundry — Fuzz Testing](https://getfoundry.sh/forge/advanced-testing/fuzz-testing)
- [Foundry — Traces](https://getfoundry.sh/forge/traces)
- [Foundry — Gas Reports](https://getfoundry.sh/forge/gas-reports)

## Siguiente paso

Completa la [evaluación final](exercises/evaluacion-final.md), revisa la [rúbrica](exercises/rubrica-final.md) y refuerza las áreas débiles. Después construye una variante local propia antes de explorar frameworks o despliegues públicos.
