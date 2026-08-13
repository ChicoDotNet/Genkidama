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

La misma aplicación crece para enseñar tipos, estado, funciones, visibilidad, errores, eventos, Ether, ABI, pruebas, tooling, seguridad y diseño de contratos.

## Estado del curso

**6/17 lecciones del piloto completadas.** El vertical actual compila y pasa su suite con Foundry en CI sobre Ubuntu 24.04; además ya cubre ABI y eventos como fronteras públicas de integración.

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

Para inspeccionar la superficie pública:

```bash
forge inspect FreelanceEscrow abi
```

## Lecciones

1. [Primer depósito y estado del escrow](lessons/01-primer-deposito-y-estado.md)
2. [Máquina de estados, roles y entrega](lessons/02-maquina-de-estados-roles-y-entrega.md)
3. [Liberación, reembolso y transferencia de valor](lessons/03-liberacion-reembolso-y-transferencia-de-valor.md)
4. [Reverts precisos, suite y checkpoint](lessons/04-reverts-precisos-suite-y-checkpoint.md)
5. [ABI: el contrato que otros programas realmente ven](lessons/05-abi-la-frontera-publica.md)
6. [Eventos como contrato observable](lessons/06-eventos-como-contrato-observable.md)

### Checkpoint 01

- [Ejercicio — Constructor seguro](exercises/checkpoint-01.md)
- [Solución de referencia](solutions/checkpoint-01.md) — ábrela sólo después de tu intento.

## Qué sabrás hacer al terminar

La meta 0 → Junior es que puedas leer un contrato sencillo, implementar y probar cambios sin una receta línea por línea, razonar sobre estado y autorización, manejar Ether y errores, usar Foundry, consultar documentación oficial, reconocer riesgos comunes y explicar las decisiones de FreelanceEscrow en una entrevista.

## Seguridad

Todos los ejemplos usan ejecución local y valores ficticios. Un ejercicio educativo verde no convierte un contrato en software listo para custodiar dinero real. Un despliegue que maneje valor real requiere threat modeling, revisión especializada, pruebas adicionales y, según el riesgo, auditoría independiente.

## Preguntas frecuentes

### ¿Necesito MetaMask?

No para el curso base. Foundry proporciona cuentas y una EVM local para pruebas.

### ¿Vamos a crear un token?

No. El proyecto se mantiene enfocado en un problema de negocio concreto: custodia y liquidación de un pago freelance.

### ¿Foundry es parte de Solidity?

No. Solidity es el lenguaje y Foundry es el toolchain elegido para compilar, probar y trabajar localmente de forma reproducible.

### ¿La ABI es lo mismo que el contrato?

No. La ABI describe la frontera codificable que consumen herramientas e integraciones; el bytecode y la lógica interna siguen siendo artefactos distintos.

### ¿Este proyecto está listo para producción?

No. Es una aplicación educativa diseñada para aprender fundamentos profesionales y practicar decisiones de seguridad.

## Glosario inicial

- **EVM:** máquina virtual que ejecuta bytecode de contratos compatibles.
- **Wei/Ether:** unidades de valor nativo; `1 ether` es una unidad de conveniencia de Solidity.
- **`msg.sender`:** dirección que realiza la llamada actual.
- **`msg.value`:** valor enviado junto con una llamada `payable`.
- **ABI:** convención para codificar llamadas, argumentos, resultados, eventos y errores.
- **selector:** primeros cuatro bytes derivados de la firma canónica de una función o error.
- **event/log:** señal observable emitida durante una ejecución exitosa.
- **custom error:** error tipado y eficiente que puede devolver datos al llamador.

## Referencias oficiales

- [Solidity documentation](https://docs.soliditylang.org/en/v0.8.35/)
- [Solidity contracts](https://docs.soliditylang.org/en/v0.8.35/contracts.html)
- [Solidity ABI specification](https://docs.soliditylang.org/en/v0.8.35/abi-spec.html)
- [Solidity security considerations](https://docs.soliditylang.org/en/v0.8.35/security-considerations.html)
- [Foundry Book](https://getfoundry.sh/)
- [Foundry — Writing Tests](https://getfoundry.sh/forge/writing-tests)

## Siguiente paso

Empieza en la [Lección 01](lessons/01-primer-deposito-y-estado.md). Después de la Lección 06, el siguiente incremento profundizará en interfaces explícitas y fronteras de seguridad antes del checkpoint 02.
