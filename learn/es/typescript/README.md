# Curso de TypeScript desde cero — Construye un gestor full-stack para freelancers

TypeScript añade un sistema de tipos estático sobre JavaScript para detectar errores antes de ejecutar el programa y mejorar el mantenimiento de aplicaciones que crecen. En este curso aprenderás desde cero construyendo **FreelanceDesk**, una aplicación full-stack local para administrar clientes, proyectos y cotizaciones.

TypeScript se usa ampliamente en frontend, backend con Node.js, tooling y aplicaciones full-stack. Su ecosistema es grande y activo, pero ningún lenguaje garantiza empleo: el objetivo aquí es producir evidencia práctica defendible para tareas Junior / Entry Level.

## ¿Puedo aprender TypeScript desde cero?

Sí. El curso explica la sintaxis necesaria y no exige haber tomado el curso de JavaScript. Aun así, TypeScript ejecuta JavaScript en runtime: aprenderás la diferencia entre lo que comprueba el compilador y lo que debe validarse cuando llegan datos externos.

## Qué vas a construir

FreelanceDesk crece sobre una sola aplicación:

- clientes con nombre y correo normalizados;
- proyectos con ciclo `planned → active → completed`;
- cotizaciones con conceptos, cantidades y precios;
- API HTTP local con Node.js;
- interfaz web que consume la API;
- persistencia JSON detrás de una frontera reemplazable;
- pruebas, tooling, diagnóstico y hardening conforme avance el curso.

A partir de la lección 08 el servidor conserva clientes, cotizaciones y proyectos en `app/data/freelance-desk.json`. El archivo se valida al cargar y una persistencia corrupta no se convierte silenciosamente en estado vacío.

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

## Lecciones

1. [Tu primera cotización tipada](lessons/01-tu-primera-cotizacion-tipada.md)
2. [Modela clientes y datos de negocio](lessons/02-modela-clientes-y-datos-de-negocio.md)
3. [Funciones, módulos y validación](lessons/03-funciones-modulos-y-validacion.md)
4. [De tipos a una aplicación full-stack](lessons/04-de-tipos-a-app-full-stack-y-checkpoint.md)
5. [Proyectos y estados tipados](lessons/05-proyectos-y-estados-tipados.md)
6. [Transiciones de proyecto y API](lessons/06-transiciones-de-proyecto-y-api.md)
7. [Una frontera de persistencia](lessons/07-frontera-de-persistencia.md)
8. [JSON confiable y Checkpoint 02](lessons/08-json-confiable-y-checkpoint.md)

## Qué sabrás hacer al terminar

El Course DoD completo llevará a poder leer y escribir TypeScript idiomático, modelar contratos, validar datos externos, trabajar con Node y navegador, persistir información, probar comportamiento, depurar, explicar arquitectura y resolver una evaluación final sin receta.

## Trabajo que usa estas habilidades

TypeScript aparece en equipos de frontend, Node.js/backend, full-stack, tooling y productos web. Un perfil junior sigue necesitando fundamentos web, HTTP, pruebas y capacidad para leer código existente; conocer tipos por sí solo no sustituye esas competencias.

## Preguntas frecuentes

### ¿Por qué no React desde la primera lección?

Porque el objetivo es aprender TypeScript. La primera versión usa Node y APIs web estándar para que tipos, módulos, HTTP y fronteras sean visibles. Un framework sólo se incorpora si aporta una ventaja didáctica o profesional clara sin ocultar el lenguaje.

### ¿Los tipos sustituyen validación?

No. Los tipos desaparecen al ejecutar JavaScript. JSON, formularios, archivos y respuestas HTTP siguen necesitando validación en runtime.

### ¿La app guarda datos al reiniciar?

Sí, desde la lección 08. Producción local usa un archivo JSON validado y las pruebas pueden inyectar otro `AppStateStore` sin tocar tus datos.

### ¿Por qué JSON y no una base de datos?

Porque la necesidad actual es enseñar una frontera real de persistencia y validación sin añadir una dependencia antes de tiempo. La interfaz del store permite migrar después a SQLite u otro motor sin mover las reglas del dominio.

## Glosario

- **tipo:** descripción estática de los valores aceptados por una expresión o contrato.
- **interface:** forma nombrada de un objeto o capacidad en TypeScript.
- **narrowing:** proceso de reducir un tipo amplio a uno específico mediante evidencia del programa.
- **runtime:** momento en que el JavaScript emitido se ejecuta.
- **frontera:** punto donde entran o salen datos externos al núcleo del programa.
- **snapshot:** representación serializable del estado en un momento determinado.

## Cómo hablar de este proyecto en una entrevista

Empieza por el problema: un freelancer necesita administrar clientes, proyectos y cotizaciones sin depender de servicios externos. Explica por qué separaste reglas puras, HTTP, DOM y persistencia; dónde TypeScript ayuda y dónde sigue siendo necesaria la validación runtime. Muestra una prueba que impide saltar estados de proyecto y otra que rechaza persistencia corrupta. Reconoce que JSON local no resuelve concurrencia multiusuario y explica por qué la frontera `AppStateStore` permite evolucionar sin acoplar el dominio.

## Referencias oficiales

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [TypeScript 6.0](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-6-0.html)
- [Node.js releases](https://nodejs.org/en/about/previous-releases)
- [Node.js HTTP](https://nodejs.org/api/http.html)
- [Node.js File system](https://nodejs.org/api/fs.html)
- [Fetch API — MDN](https://developer.mozilla.org/docs/Web/API/Fetch_API)

## Siguiente paso

Completa las lecciones 5–8 y el Checkpoint 02 antes de ampliar consultas y edición de FreelanceDesk.
