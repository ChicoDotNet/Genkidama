# Lección 17 — Evaluación final: entrega ThreadSeek

## Qué vas a conseguir

Demostrar que puedes leer, modificar, probar, medir y explicar una aplicación C++ existente sin seguir una receta paso a paso.

## Antes de empezar

Completa las lecciones 1–16 y los cuatro checkpoints. Esta evaluación no introduce conceptos nuevos: combina filesystem, value semantics, RAII, persistencia, errores, medición, concurrencia, cancelación y portabilidad.

## El problema

Aprender C++ no termina al memorizar sintaxis o lanzar threads. Un Junior útil debe poder entrar a una base existente, localizar una frontera, corregir un defecto, agregar comportamiento, protegerlo con pruebas y explicar qué complejidad decidió no introducir.

## Encargo final

Trabaja primero con el [ejercicio final versionado](../exercises/final-threadseek.md). No abras la solución de referencia hasta haber intentado las seis historias.

### Historia 1 — Filtro de extensión

Añade un filtro opcional de extensión al proceso de descubrimiento, no sólo a la búsqueda posterior. Conserva el comportamiento actual cuando no se proporciona filtro y evita duplicar lógica entre modo secuencial y paralelo.

### Historia 2 — Corrige un defecto de índice persistido

Haz que un registro persistido con datos extra al final de una línea o un tamaño fuera de rango produzca un error explícito en vez de reconstruir parcialmente un índice dudoso. Añade una prueba de regresión.

### Historia 3 — Cancelación desde una frontera real

Diseña una pequeña frontera de aplicación o CLI capaz de conservar un `std::stop_source` y solicitar cancelación sin exponerlo al núcleo del índice. No necesitas implementar señales del sistema operativo si no puedes probarlas portablemente.

### Historia 4 — Progreso útil

Extiende la información de progreso para que una interfaz pueda mostrar una razón útil entre archivos descubiertos, omitidos y entradas visitadas sin observar los contenedores internos de workers.

### Historia 5 — Experimento de rendimiento defendible

Compara 1, 2, 4 y 8 workers sobre un árbol suficientemente grande. Conserva equivalencia funcional como gate, reporta varias mediciones y explica cuándo el paralelo deja de aportar. No conviertas un número absoluto en requisito de CI.

### Historia 6 — Evolución arquitectónica

Supón que ThreadSeek debe mantener un índice incremental durante horas mientras el filesystem cambia. Diseña la frontera mínima para incorporar eventos de cambios sin convertir `FileIndex`, `IndexStore` o el descubridor en una clase gigantesca. No implementes un watcher específico de plataforma salvo que puedas justificarlo y probarlo.

## Evidencia obligatoria

Tu entrega debe incluir:

1. código C++ modificado con APIs públicas documentadas;
2. pruebas nuevas o ajustadas para los contratos y regresiones relevantes;
3. CMake configure/build y CTest verdes;
4. evidencia de GCC/Clang en Linux y MSVC en Windows cuando cambie código portable;
5. una comparación de rendimiento que preserve equivalencia y evite conclusiones exageradas;
6. una decisión que deliberadamente **no** implementaste por sobrearquitectura, dependencia de plataforma o falta de evidencia.

## Cómo comprobar

Desde `learn/es/cpp/app`:

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build --parallel
ctest --test-dir build --output-on-failure
./build/threadseek . txt
```

En Windows usa el ejecutable de la configuración `Release` correspondiente al generador elegido.

La política del repositorio no exige 100% de code coverage: cuando sea medible, 44% es piso suficiente si contratos, failure modes y regresiones relevantes están protegidos; 44%–72.8% es plenamente aceptable y una cifra superior es bienvenida.

## Rúbrica — 100 puntos

| Área | Puntos | Evidencia esperada |
|---|---:|---|
| C++ idiomático y ownership | 20 | value semantics, RAII, STL y lifetime claros |
| Comportamiento y pruebas | 20 | contratos y regresiones protegidos con tests útiles |
| Filesystem y persistencia | 15 | fallos explícitos y tolerancia razonada a mutaciones |
| Concurrencia y cancelación | 15 | `jthread`/stop tokens y mínimo estado compartido |
| Medición y rendimiento | 10 | comparación reproducible sin thresholds frágiles |
| Portabilidad y build | 10 | CMake y evidencia GCC/Clang/MSVC coherente |
| Explicación profesional | 10 | decisiones, alternativas y trade-offs defendibles |

### Interpretación

- **85–100:** evidencia sólida de preparación Junior/Entry Level para este alcance.
- **70–84:** base razonable; repasa las áreas de menor puntuación antes de presentar el proyecto.
- **<70:** vuelve al checkpoint correspondiente y repite la historia que expuso la brecha.

La rúbrica mide preparación sobre ThreadSeek; no promete empleo.

## Cómo hablar de este proyecto en una entrevista

Prepárate para responder con ejemplos del repositorio:

1. ¿Por qué el índice no depende directamente de CMake, CLI o persistencia?
2. ¿Qué problema resuelve RAII en `std::jthread` y streams?
3. ¿Por qué ThreadSeek ordena los resultados después de combinar workers?
4. ¿Qué diferencia existe entre una raíz inválida y un archivo que desaparece durante el scan?
5. ¿Por qué la cancelación es cooperativa y no una terminación forzada?
6. ¿Qué estado realmente necesita ser compartido entre workers?
7. ¿Por qué paralelo más rápido no es un contrato de negocio?
8. ¿Qué reveló MSVC que GCC/Clang no revelaron en el primer pase?
9. ¿Cómo evolucionarías hacia indexación incremental sin rehacer todo el núcleo?

No memorices respuestas. Señala decisiones que puedas enseñar en el código y sus tests.

## Solución de referencia

Sólo después de intentar el ejercicio, compara tus decisiones con la [solución de referencia](../solutions/final-threadseek.md). No necesitas coincidir con ella si conservas los contratos, pruebas el comportamiento y puedes explicar tus trade-offs.

## Referencias

- https://en.cppreference.com/w/cpp/filesystem
- https://en.cppreference.com/w/cpp/thread/jthread
- https://cmake.org/documentation/
- https://gcc.gnu.org/onlinedocs/
- https://learn.microsoft.com/cpp/

## Siguiente paso

Si tu entrega satisface la rúbrica y los gates ejecutables, conserva ThreadSeek como proyecto de portafolio y practica explicarlo desde el problema hacia las decisiones técnicas. Para control de versiones utiliza el [curso transversal de Git](../../git/README.md).
