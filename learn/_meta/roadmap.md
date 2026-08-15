# Roadmap de Genkidama Learn

## Principio de planificación

Calidad antes de cantidad. No se abren muchos cursos para mostrar actividad.

Un curso incompleto se retoma antes de iniciar otro salvo que exista una razón técnica o de independencia claramente documentada.

## Fase 0 — Fundación compartida

Objetivo:

- pedagogía;
- especificación;
- guía de autoría;
- catálogo;
- progreso persistente;
- decisiones;
- validación automática;
- navegación desde el README raíz.

No se crean esqueletos vacíos de cursos.

## Curso transversal — Git

Git se enseña como curso transversal independiente de los 45 lenguajes V1. **ReleaseDesk está completo en 17/17 lecciones** y funciona como infraestructura pedagógica compartida: los cursos de lenguaje lo recomiendan cuando necesitan control de versiones, ramas, colaboración, diagnóstico o recuperación en vez de duplicar mini-cursos de Git.

Su estado se conserva en `progress.yml` bajo `transversal_courses` y no altera el denominador de 45 lenguajes.

## Fase 1 — Pilotos

**Estado: completa.** Los cinco pilotos terminaron su Course DoD en español y ya probaron la plantilla sobre ecosistemas deliberadamente distintos:

1. C#
2. Python
3. JavaScript
4. COBOL
5. Solidity

Los pilotos estabilizaron:

- longitud real de lecciones;
- densidad de contenido;
- formato escrito/video;
- ejercicios;
- soluciones;
- checkpoints;
- evaluación;
- metadata;
- CI;
- experiencia Windows/Linux cuando corresponde.

TypeScript, Java, Go, Rust, PHP y Kotlin ya completaron también su Course DoD como cursos posteriores a los pilotos. Kotlin cerró la evidencia Android prometida con un módulo compilable Room/Compose sobre AGP 9.3 y Built-in Kotlin. **Swift está ahora en progreso en 12/17 lecciones** con TimeQuote como aplicación canónica: el núcleo SwiftPM portable ya tiene fronteras de casos de uso, repositorio en memoria y persistencia JSON durable probada entre instancias. El siguiente incremento seguirá el mismo PR con concurrencia/estado antes de abrir C++.

## Orden v1 provisional

Este orden es una hipótesis de producción, no un ranking universal de empleabilidad.

Antes de iniciar cada curso se revalida brevemente demanda, tooling, oportunidad de contenido y aplicación canónica. El orden puede cambiar y la decisión se registra.

1. **C#** (`csharp`)
2. **Python** (`python`)
3. **JavaScript** (`javascript`)
4. **COBOL** (`cobol`)
5. **Solidity** (`solidity`)
6. **TypeScript** (`typescript`)
7. **Java** (`java`)
8. **Go** (`go`)
9. **Rust** (`rust`)
10. **PHP** (`php`)
11. **Kotlin** (`kotlin`)
12. **Swift** (`swift`)
13. **C++** (`cpp`)
14. **PowerShell** (`powershell`)
15. **Ruby** (`ruby`)
16. **Dart** (`dart`)
17. **C** (`c`)
18. **Visual Basic .NET** (`vbnet`)
19. **F#** (`fsharp`)
20. **R** (`r`)
21. **Julia** (`julia`)
22. **HTML** (`html`)
23. **Shell** (`shell`)
24. **Elixir** (`elixir`)
25. **Erlang** (`erlang`)
26. **Scala** (`scala`)
27. **Clojure** (`clojure`)
28. **Haskell** (`haskell`)
29. **OCaml** (`ocaml`)
30. **Lua** (`lua`)
31. **Perl** (`perl`)
32. **Groovy** (`groovy`)
33. **Fortran** (`fortran`)
34. **Ada** (`ada`)
35. **Pascal** (`pascal`)
36. **Objective-C** (`objective-c`)
37. **Nim** (`nim`)
38. **Crystal** (`crystal`)
39. **Zig** (`zig`)
40. **MATLAB** (`matlab`)
41. **GDScript** (`gdscript`)
42. **Assembly** (`assembly`)
43. **Common Lisp** (`common-lisp`)
44. **Prolog** (`prolog`)
45. **VBA** (`vba`)

## Criterios para mover el orden

Se puede adelantar o retrasar un curso por:

- impacto laboral;
- demanda de búsqueda;
- oportunidad de contenido útil;
- diversidad de paradigma;
- facilidad o limitación de CI;
- interés educativo;
- ausencia de buenos recursos actuales;
- aprendizaje obtenido en pilotos.

No se altera el concepto central de Solidity: `FreelanceEscrow`.

## Expansión posterior a v1

Registrados, no comprometidos como parte de los 45 actuales:

- Delphi
- GNU Octave
- SQL
- CSS
- MicroPython
- Rockstar

Rockstar se presenta como ejercicio pedagógico/esotérico, no como una ruta de empleo con demanda significativa.

## Traducciones

Sólo después de que la estructura y varios cursos en español estén maduros:

1. `en`
2. `zh-Hans`
3. `ja`
4. `fr`
5. `it`
6. `pt-BR`
7. `ru`
8. `de`

Español (`es`) permanece como fuente de verdad inicial.

## Después de v1

Fase separada:

- traducciones;
- actualización periódica;
- nuevos lenguajes;
- mejoras SEO/AEO/GEO;
- posible sitio estático;
- cursos complementarios;
- mantenimiento y evolución del curso transversal de Git.
