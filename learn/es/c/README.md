# Curso de C desde cero — Construye un logger binario de telemetría

C es un lenguaje compilado pequeño y explícito que sigue siendo central en sistemas operativos, firmware, bibliotecas nativas y software donde el formato de memoria y los límites importan. Este curso construye una sola aplicación: **TelemetryTape**, un logger binario que registra muestras con un formato versionado y luego las analiza desde una CLI.

## Qué vas a construir

TelemetryTape crea archivos `.gtl`, agrega registros de telemetría y los lee sin depender del layout de una `struct` en memoria. La API pública separa serialización, I/O y presentación; la CLI hace ownership de sus buffers, los resúmenes y consultas procesan por streaming, la exportación CSV reutiliza el mismo contrato de filtros y el bloque operacional permite diagnosticar y recuperar hacia una copia sin modificar el origen.

Al terminar podrás explicar y modificar una base C pequeña con tipos de ancho fijo, memoria dinámica explícita, errores observables, formato binario portable, CMake/CTest, consultas streaming y recuperación no destructiva.

## Qué necesitas

- Un compilador con soporte C23 suficiente para este proyecto. CI certificado ejecuta GCC 13.3.0 y Clang 18.1.3 sobre Ubuntu 24.04.4 LTS.
- CMake 3.25 o posterior; CI ejecutó CMake 3.31.6.
- Ninja o el generador de CMake disponible en tu sistema.
- VS Code es opcional.

No necesitas experiencia previa con C. Sí conviene saber usar una terminal básica; para control de versiones usa el [curso transversal de Git](../git/).

GCC documenta soporte de C23 y desde GCC 15 lo usa como dialecto predeterminado; este proyecto pasa `C_STANDARD 23` explícitamente para que el contrato no dependa del default del compilador.

## Build, test, run e instalación

```bash
cd learn/es/c/app
cmake -S . -B build -G Ninja -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
./build/telemetry_cli init sample.gtl
./build/telemetry_cli log sample.gtl 1000 7 21500 0
./build/telemetry_cli query sample.gtl 7 '*' '*'
./build/telemetry_cli summary sample.gtl
./build/telemetry_cli diagnose sample.gtl
./build/telemetry_cli export sample.gtl report.csv 7 '*' '*'
cmake --install build --prefix dist
```

En Windows usa el generador CMake/compilador que tengas instalado; la evidencia automatizada actual del curso certifica GCC y Clang sobre Ubuntu, así que no se presenta MSVC como plataforma probada en este curso.

## Avance

**17/17 lecciones implementadas — Course DoD completo.**

1. [Compila C y ejecuta TelemetryTape](lessons/01-compila-y-ejecuta-telemetrytape.md)
2. [Modela telemetría con tipos de ancho fijo](lessons/02-modela-telemetria-con-tipos-fijos.md)
3. [Diseña un formato binario portable](lessons/03-disena-un-formato-binario-portable.md)
4. [Haz explícitos los errores y prueba comportamiento](lessons/04-errores-explicitos-y-pruebas.md)
5. [Cuenta antes de reservar memoria](lessons/05-cuenta-antes-de-reservar-memoria.md)
6. [Resume sin cargar todo el archivo](lessons/06-resume-sin-cargar-todo.md)
7. [Distingue versión incompatible de datos truncados](lessons/07-distingue-version-y-truncado.md)
8. [Centraliza el parser y protege regresiones](lessons/08-centraliza-parser-y-protege-regresiones.md)
9. [Filtra por sensor sin copiar el archivo](lessons/09-filtra-por-sensor-sin-copiar.md)
10. [Consulta intervalos temporales sin ambigüedad](lessons/10-consulta-intervalos-temporales.md)
11. [Exporta CSV de forma reproducible](lessons/11-exporta-csv-reproducible.md)
12. [No prolongues un archivo corrupto](lessons/12-no-prolongues-un-archivo-corrupto.md)
13. [Diagnostica sin modificar el archivo](lessons/13-diagnostica-sin-modificar-el-archivo.md)
14. [Depura con evidencia reproducible](lessons/14-depura-con-evidencia-reproducible.md)
15. [Recupera sin sobrescribir el original](lessons/15-recupera-sin-sobrescribir-el-original.md)
16. [Instala y entrega un binario portable](lessons/16-instala-y-entrega-un-binario-portable.md)
17. [Demuestra que puedes mantener TelemetryTape](lessons/17-demuestra-que-puedes-mantener-telemetrytape.md)

[Checkpoint 01 — Archivo confiable](exercises/checkpoint-01.md) · [Solución](solutions/checkpoint-01.md)

[Checkpoint 02 — Analiza sin perder control de memoria](exercises/checkpoint-02.md) · [Solución](solutions/checkpoint-02.md)

[Checkpoint 03 — Consulta sin romper el contrato](exercises/checkpoint-03.md) · [Solución](solutions/checkpoint-03.md)

[Checkpoint 04 — Diagnostica, recupera y entrega](exercises/checkpoint-04.md) · [Solución](solutions/checkpoint-04.md)

[Evaluación final — Mantén y evoluciona TelemetryTape](exercises/final-assessment.md) · [Solución de referencia](solutions/final-assessment.md)

## Qué sabrás hacer al terminar

- leer y escribir C pequeño con tipos explícitos y flujo de control claro;
- modelar formatos binarios sin depender del layout de memoria de una `struct`;
- razonar sobre ownership, `malloc`/`free` y límites de buffers;
- separar errores de entrada, formato y almacenamiento;
- procesar archivos mediante streaming;
- escribir y ejecutar pruebas con CTest;
- construir e instalar con CMake;
- diagnosticar corrupción sin modificar el origen;
- recuperar sólo datos demostrablemente válidos hacia otra ruta;
- extender una aplicación existente y explicar trade-offs técnicos.

## Aplicación final

TelemetryTape es deliberadamente pequeño y local. No pretende competir con una plataforma de observabilidad completa: demuestra fundamentos transferibles a sistemas, firmware, bibliotecas nativas y herramientas que consumen formatos binarios.

## Preguntas frecuentes

### ¿C y C++ son lo mismo?

No. Comparten historia y parte de la sintaxis, pero son lenguajes distintos con modelos y ecosistemas diferentes. Este curso trabaja C y no presupone C++.

### ¿Por qué no escribir la `struct` directamente al archivo?

Porque padding, alineación, endianness y representación pueden variar. TelemetryTape define bytes explícitos para que el archivo tenga un contrato estable.

### ¿Por qué el curso usa streaming?

Porque contar, resumir o filtrar no debería obligar a reservar memoria proporcional al tamaño completo del archivo cuando el resultado puede producirse registro por registro.

### ¿La recuperación arregla el archivo original?

No. Produce otra ruta con el prefijo que pudo demostrarse válido. Preservar el origen mantiene la operación reversible y facilita investigar el defecto.

### ¿Completar el curso garantiza empleo?

No. Te da una base práctica 0 → Junior sobre este tipo de problemas; muchos puestos de C requieren además sistemas operativos, debugging, arquitectura de computadores, redes o dominio embebido.

## Glosario

- **ABI:** contrato binario entre componentes; puede incluir layout, calling convention y representación.
- **endianness:** orden de los bytes que representan un valor multibyte.
- **ownership:** responsabilidad de administrar y liberar un recurso.
- **buffer:** región de memoria usada para almacenar temporalmente datos.
- **streaming:** procesamiento incremental sin cargar el dataset completo.
- **magic:** bytes iniciales que identifican el tipo/formato del archivo.
- **truncado:** archivo que termina antes de completar una estructura esperada.
- **intervalo semiabierto `[start,end)`:** incluye el inicio y excluye el final.
- **CTest:** runner de pruebas integrado con CMake.
- **warnings-as-errors:** política que convierte advertencias del compilador en fallos del build.

## Cómo hablar de este proyecto en una entrevista

Describe primero el problema y después los contratos. Una explicación breve y concreta puede cubrir:

- por qué el formato `.gtl` está versionado y serializado byte a byte;
- cómo evitas cargar archivos completos para queries/resúmenes;
- dónde vive el ownership de la memoria;
- cómo distingues magic inválido, versión futura y truncado;
- por qué el recovery escribe a otra ruta;
- qué validan GCC, Clang, CTest y el smoke test;
- qué mejorarías si el volumen creciera mucho.

No presentes el proyecto como experiencia que no tienes. Explica qué construiste, qué probaste y qué límites reconoces.

## Git

Para ramas, recuperación, colaboración e historial usa el [curso transversal de Git](../git/). Aquí sólo tratamos Git cuando es inevitable para obtener o ejecutar el proyecto.

## Referencias oficiales

- [Soporte de estándares C en GCC](https://gcc.gnu.org/projects/c-status.html)
- [Releases de GCC](https://gcc.gnu.org/releases.html)
- [Documentación de CMake](https://cmake.org/documentation/)
- [Propiedad `C_STANDARD` de CMake](https://cmake.org/cmake/help/latest/prop_tgt/C_STANDARD.html)
- [cppreference C](https://en.cppreference.com/w/c)

## Siguiente paso

Resuelve la evaluación final sin mirar la solución. Después reconstruye el proyecto desde una copia limpia y explica las decisiones principales en voz alta. El roadmap de Genkidama Learn continúa con **Visual Basic .NET**.