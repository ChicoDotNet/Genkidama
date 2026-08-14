# Curso de TypeScript desde cero — Construye un gestor full-stack para freelancers

TypeScript añade un sistema de tipos estático sobre JavaScript para detectar errores antes de ejecutar el programa y mejorar el mantenimiento de aplicaciones que crecen. En este curso aprenderás desde cero construyendo **FreelanceDesk**, una aplicación full-stack local para administrar clientes, proyectos y cotizaciones.

TypeScript se usa ampliamente en frontend, backend con Node.js, tooling y aplicaciones full-stack. GitHub reportó que en agosto de 2025 se convirtió en el lenguaje con más contribuidores de su plataforma; eso no garantiza empleo, pero sí confirma un ecosistema grande y activo.

## ¿Puedo aprender TypeScript desde cero?

Sí. El curso explica la sintaxis necesaria y no exige haber tomado el curso de JavaScript. Aun así, TypeScript ejecuta JavaScript en runtime: aprenderás la diferencia entre lo que comprueba el compilador y lo que debe validarse cuando llegan datos externos.

## Qué vas a construir

FreelanceDesk crece sobre una sola aplicación:

- clientes con nombre y correo normalizados;
- proyectos y estado de trabajo en incrementos posteriores;
- cotizaciones con conceptos, cantidades y precios;
- API HTTP local con Node.js;
- interfaz web que consume la API;
- persistencia, pruebas, tooling, diagnóstico y hardening conforme avance el curso.

El primer incremento ya permite crear clientes y cotizaciones desde el navegador. El estado todavía vive en memoria; la persistencia se incorpora cuando exista una necesidad visible.

## Toolchain objetivo

- Node.js 24 LTS (Krypton).
- TypeScript 6.0 estable.
- npm.
- Navegador moderno.

No usamos TypeScript 7 previews/beta: el curso sigue la política de versiones estables soportadas.

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

Abre `http://localhost:3000`.

## Lecciones

1. [Tu primera cotización tipada](lessons/01-tu-primera-cotizacion-tipada.md)
2. [Modela clientes y datos de negocio](lessons/02-modela-clientes-y-datos-de-negocio.md)
3. [Funciones, módulos y validación](lessons/03-funciones-modulos-y-validacion.md)
4. [De tipos a una aplicación full-stack](lessons/04-de-tipos-a-app-full-stack-y-checkpoint.md)

## Qué sabrás hacer al terminar

El Course DoD completo llevará a poder leer y escribir TypeScript idiomático, modelar contratos, validar datos externos, trabajar con Node y navegador, persistir información, probar comportamiento, depurar, explicar arquitectura y resolver una evaluación final sin receta.

## Trabajo que usa estas habilidades

TypeScript aparece en equipos de frontend, Node.js/backend, full-stack, tooling y productos web. Un perfil junior sigue necesitando fundamentos web, HTTP, pruebas y capacidad para leer código existente; conocer tipos por sí solo no sustituye esas competencias.

## Preguntas frecuentes

### ¿Por qué no React desde la primera lección?

Porque el objetivo es aprender TypeScript. La primera versión usa Node y APIs web estándar para que los tipos, módulos, HTTP y fronteras sean visibles. Un framework sólo se incorpora si aporta una ventaja didáctica o profesional clara sin ocultar el lenguaje.

### ¿Los tipos sustituyen validación?

No. Los tipos desaparecen al ejecutar JavaScript. JSON, formularios, archivos y respuestas HTTP siguen necesitando validación en runtime.

### ¿La app guarda datos al reiniciar?

Todavía no. El primer vertical usa memoria deliberadamente; la persistencia llegará en un incremento posterior y entonces tendrá pruebas y contrato explícitos.

## Glosario

- **tipo:** descripción estática de los valores aceptados por una expresión o contrato.
- **interface:** forma nombrada de un objeto en TypeScript.
- **narrowing:** proceso de reducir un tipo amplio a uno más específico mediante evidencia del programa.
- **runtime:** momento en que el JavaScript emitido se ejecuta.
- **frontera:** punto donde entran o salen datos externos al núcleo del programa.

## Cómo hablar de este proyecto en una entrevista

Empieza por el problema: un freelancer necesita administrar clientes y cotizaciones sin depender de servicios externos. Explica después por qué separaste reglas puras, HTTP y DOM; dónde TypeScript ayuda y dónde sigue siendo necesaria la validación runtime. Muestra una prueba de regresión y reconoce la limitación actual de persistencia en memoria.

## Referencias oficiales

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [TypeScript 6.0](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-6-0.html)
- [Node.js releases](https://nodejs.org/en/about/previous-releases)
- [Node.js HTTP](https://nodejs.org/api/http.html)
- [Fetch API — MDN](https://developer.mozilla.org/docs/Web/API/Fetch_API)

## Siguiente paso

Completa las cuatro primeras lecciones y el Checkpoint 01 antes de ampliar FreelanceDesk.
