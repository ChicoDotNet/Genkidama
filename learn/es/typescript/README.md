# Curso de TypeScript desde cero — Construye un gestor full-stack para freelancers

TypeScript añade un sistema de tipos estático sobre JavaScript para detectar errores antes de ejecutar el programa y mejorar el mantenimiento de aplicaciones que crecen. En este curso aprenderás desde cero construyendo **FreelanceDesk**, una aplicación full-stack local para administrar clientes, proyectos y cotizaciones.

TypeScript se usa ampliamente en frontend, backend con Node.js, tooling y aplicaciones full-stack. Su ecosistema es grande y activo, pero ningún lenguaje garantiza empleo: el objetivo aquí es producir evidencia práctica defendible para tareas Junior / Entry Level.

## ¿Puedo aprender TypeScript desde cero?

Sí. El curso explica la sintaxis necesaria y no exige haber tomado el curso de JavaScript. TypeScript ejecuta JavaScript en runtime: aprenderás la diferencia entre lo que comprueba el compilador y lo que debe validarse cuando llegan datos externos.

## Qué vas a construir

FreelanceDesk crece sobre una sola aplicación:

- clientes con nombre y correo normalizados;
- proyectos con ciclo `planned → active → completed` y consultas por cliente/estado;
- cotizaciones con conceptos, subtotal y ciclo `draft → sent → accepted|rejected`;
- API HTTP local con Node.js;
- interfaz web que consume la API;
- persistencia JSON detrás de una frontera reemplazable;
- mutaciones durables que no adelantan memoria cuando la persistencia falla;
- contratos HTTP explícitos, límite de cuerpos JSON y headers defensivos;
- diagnóstico agregado opt-in sin URLs, cuerpos ni datos personales;
- pruebas y un gate reproducible de type-check, build y test;
- evaluación final autónoma que exige modificar la misma base sin receta.

Desde la lección 08 el servidor conserva clientes, cotizaciones y proyectos en `app/data/freelance-desk.json`. Desde la lección 11 una escritura fallida devuelve `503` y conserva el estado previo en memoria. Desde la lección 16 los cuerpos JSON están limitados a 64 KiB por defecto y deben declarar `Content-Type: application/json`.

## Toolchain objetivo

- Node.js 24 LTS (Krypton).
- TypeScript 6.0 estable.
- npm.
- Navegador moderno.

No usamos TypeScript 7 preview/beta: el curso sigue la política de versiones estables soportadas.

## Instalar

Desde `app/`:

```bash
npm install
```

## Build

```bash
npm run build
```

## Test

```bash
npm test
```

## Verificación completa

```bash
npm run verify
```

## Run

```bash
npm start
```

Abre `http://localhost:3000`. Puedes cambiar el archivo de datos mediante `FREELANCEDESK_DATA_FILE`.

Para habilitar temporalmente el diagnóstico agregado:

```bash
FREELANCEDESK_DIAGNOSTICS=1 npm start
```

Entonces `/api/diagnostics` expone sólo conteos y duraciones agregadas. Sin esa variable el endpoint responde 404.

## Lecciones

1. [Tu primera cotización tipada](lessons/01-tu-primera-cotizacion-tipada.md)
2. [Modela clientes y datos de negocio](lessons/02-modela-clientes-y-datos-de-negocio.md)
3. [Funciones, módulos y validación](lessons/03-funciones-modulos-y-validacion.md)
4. [De tipos a una aplicación full-stack](lessons/04-de-tipos-a-app-full-stack-y-checkpoint.md)
5. [Proyectos y estados tipados](lessons/05-proyectos-y-estados-tipados.md)
6. [Transiciones de proyecto y API](lessons/06-transiciones-de-proyecto-y-api.md)
7. [Una frontera de persistencia](lessons/07-frontera-de-persistencia.md)
8. [JSON confiable y Checkpoint 02](lessons/08-json-confiable-y-checkpoint.md)
9. [Consultas tipadas](lessons/09-consultas-tipadas.md)
10. [Ciclo comercial de cotizaciones](lessons/10-ciclo-comercial-de-cotizaciones.md)
11. [Mutaciones durables y fallas asíncronas](lessons/11-mutaciones-durables-y-fallas-asincronas.md)
12. [Contratos de error y Checkpoint 03](lessons/12-contratos-de-error-y-checkpoint-03.md)
13. [Errores HTTP y fronteras explícitas](lessons/13-errores-http-y-fronteras-explicitas.md)
14. [Tooling y gate profesional](lessons/14-tooling-y-gate-profesional.md)
15. [Diagnóstico y rendimiento con evidencia](lessons/15-diagnostico-y-rendimiento-con-evidencia.md)
16. [Hardening HTTP y Checkpoint 04](lessons/16-hardening-http-y-checkpoint-04.md)
17. [Evaluación final sin receta](lessons/17-evaluacion-final.md)

## Evaluación final

La [evaluación final](exercises/evaluacion-final.md) extiende FreelanceDesk con fecha objetivo y consulta de proyectos vencidos, exige resolver una ambigüedad de IDs, conservar durabilidad y hardening, escribir regresiones y consultar documentación oficial. No prescribe archivos ni líneas.

Usa la [rúbrica final](exercises/rubrica-final.md) para autoevaluarte y abre la [solución de referencia](solutions/evaluacion-final.md) sólo después de un intento.

## Qué sabrás hacer al terminar

Al completar el Course DoD deberías poder leer y escribir TypeScript idiomático, modelar contratos, validar datos externos, trabajar con Node y navegador, persistir información, probar comportamiento y caminos de falla, depurar, explicar arquitectura y resolver una evaluación final sin receta.

## Trabajo que usa estas habilidades

TypeScript aparece en equipos de frontend, Node.js/backend, full-stack, tooling y productos web. Un perfil junior sigue necesitando fundamentos web, HTTP, pruebas y capacidad para leer código existente; conocer tipos por sí solo no sustituye esas competencias.

## Preguntas frecuentes

### ¿Por qué no React desde la primera lección?

Porque el objetivo es aprender TypeScript. La versión actual usa Node y APIs web estándar para que tipos, módulos, HTTP y fronteras sean visibles. Un framework sólo se incorpora si aporta una ventaja didáctica o profesional clara sin ocultar el lenguaje.

### ¿Los tipos sustituyen validación?

No. Los tipos desaparecen al ejecutar JavaScript. JSON, formularios, archivos, query strings y respuestas HTTP siguen necesitando validación en runtime.

### ¿La app guarda datos al reiniciar?

Sí. Producción local usa un archivo JSON validado y las pruebas pueden inyectar otro `AppStateStore` sin tocar tus datos.

### ¿Qué ocurre si falla el archivo al guardar?

La API responde `503` y no aplica el snapshot candidato a la memoria del proceso. Esto preserva consistencia local; no convierte el archivo JSON en una base de datos multiusuario.

### ¿Por qué JSON y no una base de datos?

Porque satisface la necesidad actual sin añadir una dependencia antes de tiempo. La interfaz del store permite migrar después a SQLite u otro motor sin mover las reglas del dominio.

### ¿El diagnóstico registra mis clientes o cotizaciones?

No. `RequestMetrics` sólo agrega cantidad de peticiones, fallas y duraciones. El endpoint además está deshabilitado por defecto. Una aplicación desplegada necesitaría una política de observabilidad más completa, pero recolectar datos sensibles “por si acaso” no es esa política.

### ¿Estos headers vuelven segura la aplicación?

No. CSP, `nosniff`, límites de body y validación de media type reducen superficie. No sustituyen autenticación, autorización, TLS, gestión de secretos ni revisión de seguridad cuando el sistema crece.

### ¿Completar el curso garantiza empleo?

No. Produce una base y evidencia práctica para comenzar a competir por tareas Junior / Entry Level. La contratación depende también de experiencia, comunicación, mercado, entrevistas y capacidad para seguir aprendiendo.

## Glosario

- **tipo:** descripción estática de los valores aceptados por una expresión o contrato.
- **interface:** forma nombrada de un objeto o capacidad en TypeScript.
- **narrowing:** reducción de un tipo amplio mediante evidencia de runtime.
- **runtime:** momento en que el JavaScript emitido se ejecuta.
- **frontera:** punto donde entran o salen datos externos al núcleo.
- **snapshot:** representación serializable del estado en un momento determinado.
- **durabilidad:** aquí, condición de considerar confirmada una mutación sólo después de que la persistencia la acepta.
- **hardening:** controles adicionales que reducen superficie de falla/ataque sin afirmar seguridad absoluta.

## Cómo hablar de este proyecto en una entrevista

Empieza por el problema: un freelancer necesita administrar clientes, proyectos y cotizaciones sin depender de servicios externos. Explica por qué separaste reglas puras, HTTP, DOM y persistencia; dónde TypeScript ayuda y dónde sigue siendo necesaria la validación runtime. Muestra una prueba de ciclo de estados y la regresión donde `save()` falla sin dejar memoria adelantada. Explica también por qué limitas cuerpos antes de persistir y por qué el diagnóstico es opt-in y no guarda PII. Reconoce que JSON local no resuelve concurrencia multiusuario y explica por qué `AppStateStore` permite evolucionar la infraestructura sin acoplar el dominio.

Preguntas probables:

- ¿Qué diferencia hay entre tipos estáticos y validación runtime?
- ¿Por qué usar `unknown` en una frontera externa?
- ¿Cómo modelaste estados y transiciones inválidas?
- ¿Cómo aseguras que una falla de persistencia no deja memoria adelantada?
- ¿Por qué el dominio no conoce códigos HTTP?
- ¿Qué limitaciones tiene JSON frente a una base multiusuario?
- ¿Qué medirías antes de optimizar?
- ¿Qué información deliberadamente no registras en diagnóstico?

## Referencias oficiales

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [TypeScript narrowing](https://www.typescriptlang.org/docs/handbook/2/narrowing.html)
- [TypeScript 6.0](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-6-0.html)
- [Node.js releases](https://nodejs.org/en/about/previous-releases)
- [Node.js HTTP](https://nodejs.org/api/http.html)
- [Node.js File system](https://nodejs.org/api/fs.html)
- [Node.js Performance Measurement APIs](https://nodejs.org/api/perf_hooks.html)
- [Fetch API — MDN](https://developer.mozilla.org/docs/Web/API/Fetch_API)
- [HTTP status codes — MDN](https://developer.mozilla.org/docs/Web/HTTP/Status)
- [Content Security Policy — MDN](https://developer.mozilla.org/docs/Web/HTTP/CSP)

## Siguiente paso

Completa la evaluación final sin receta. Después conserva FreelanceDesk como evidencia, repite las áreas débiles de la rúbrica y construye una variante propia antes de incorporar frameworks o infraestructura por inercia.
