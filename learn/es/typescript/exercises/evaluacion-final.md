# Evaluación final — Evoluciona FreelanceDesk sin receta

Trabaja sobre la aplicación canónica. No abras la solución hasta completar un intento.

## Historia A — Fecha objetivo de proyecto

Agrega una fecha objetivo opcional a los proyectos.

Requisitos:

- un proyecto puede no tener fecha objetivo;
- cuando existe, el formato externo debe ser una fecha calendario ISO `YYYY-MM-DD` válida;
- la regla se valida fuera del DOM y no depende del servidor HTTP;
- la fecha sobrevive persistencia JSON y recarga;
- datos anteriores sin el campo siguen cargando;
- la API permite crear proyectos con o sin fecha objetivo;
- agrega pruebas para fecha válida, inválida y compatibilidad legacy.

No se prescribe el nombre de la propiedad, función, archivo ni abstracción exacta.

## Historia B — Consulta de proyectos vencidos

Añade una consulta que permita obtener proyectos vencidos respecto de una fecha de referencia explícita.

Debe cumplir:

- sólo incluye proyectos con fecha objetivo anterior a la fecha de referencia;
- un proyecto `completed` no se considera vencido;
- no uses el reloj global dentro de la regla de dominio: la fecha de referencia entra como dato;
- la consulta no muta proyectos;
- el endpoint o mecanismo HTTP debe validar la fecha de referencia recibida.

Incluye al menos una prueba determinista con una fecha fija.

## Historia C — Bugfix de IDs duplicados

Hoy dos entradas pueden intentar crear proyectos con el mismo `id`. Define e implementa una política segura y explícita.

La solución debe:

- impedir estados con dos proyectos que compartan ID;
- producir un error útil para el consumidor;
- no persistir ni publicar un snapshot parcial cuando el duplicado se rechaza;
- conservar el estado anterior si `save()` falla;
- incluir una prueba de regresión.

Documenta en una frase por qué elegiste rechazar, regenerar o reconciliar el ID. No aceptes duplicados silenciosamente.

## Historia D — Conserva contratos profesionales

Demuestra que siguen funcionando:

- `npm run verify`;
- strict type-check sin `any` para silenciar fronteras;
- creación y consulta de clientes, proyectos y cotizaciones;
- transiciones de proyecto y cotización;
- persistencia JSON compatible;
- rollback lógico ante falla de `save()`;
- respuestas `413` y `415` existentes;
- headers defensivos;
- diagnóstico opt-in sin PII;
- app shell HTTP.

No debilites una validación, un tipo o una prueba para conseguir verde.

## Historia E — Consulta documentación y diseña la siguiente evolución

Consulta al menos dos fuentes oficiales relacionadas con decisiones reales de tu cambio. Pueden ser TypeScript, Node.js o MDN. Entrega una nota breve por fuente con:

1. enlace;
2. qué verificaste;
3. qué decisión tomaste a partir de ello.

Después escribe entre 220 y 350 palabras respondiendo:

- ¿Por qué una fecha externa sigue necesitando validación aunque la propiedad sea `string` tipada?
- ¿Dónde debe vivir la regla de “vencido” y por qué recibe la fecha de referencia como argumento?
- ¿Qué riesgo aparece si dos procesos escriben el mismo JSON?
- ¿Qué contrato de `AppStateStore` cambiarías primero para migrar a concurrencia multiusuario?
- ¿Qué medirías antes de optimizar consultas con miles de proyectos?
- ¿Qué datos evitarías registrar durante ese diagnóstico?

## Entrega

Entrega:

- código;
- pruebas nuevas o modificadas;
- comandos ejecutados y resultados;
- comprobación manual relevante;
- nota de documentación oficial;
- respuesta de diseño;
- un error real que encontraste durante el trabajo y cómo lo diagnosticastes.

## Comprobación mínima

Desde `app/`:

```bash
npm run verify
npm start
```

Después prueba manualmente:

1. crear proyecto sin fecha;
2. crear proyecto con fecha válida;
3. rechazar una fecha imposible;
4. consultar vencidos con una fecha de referencia fija;
5. intentar un ID duplicado y confirmar que el estado previo sigue intacto;
6. reiniciar y comprobar persistencia;
7. provocar o simular un fallo de persistencia y confirmar que memoria no queda adelantada.

Evalúate con [`rubrica-final.md`](rubrica-final.md).
