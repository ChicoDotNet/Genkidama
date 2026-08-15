# Curso de PHP desde cero — Construye una agenda web de citas

PHP es un lenguaje de propósito general especialmente orientado al desarrollo web. En este curso aprenderás **PHP desde cero construyendo AgendaPHP**, una agenda local para registrar, editar, consultar, exportar y cancelar citas sin cruces de horario.

El objetivo no es prometer empleo. Al terminar deberás poder leer y modificar PHP moderno, ejecutar una aplicación web local, trabajar con Composer, pruebas y persistencia, explicar sus fronteras y resolver una evaluación sin tutorial paso a paso.

## Qué vas a construir

AgendaPHP crece sobre una sola base de código. Actualmente permite:

- registrar cliente, servicio, inicio y duración;
- validar tipos y reglas de dominio;
- impedir citas que se traslapan;
- guardar el calendario en JSON local;
- localizar, editar y cancelar citas por identidad estable;
- consultar por fecha y texto de servicio sin duplicar estado;
- mostrar cantidad de citas y minutos reservados derivados de la misma consulta;
- descargar el subconjunto visible como CSV con escaping estándar;
- distinguir entrada inválida (422) de almacenamiento temporalmente no disponible (503);
- probar dominio, aplicación, persistencia, CSV y HTTP con PHPUnit + smoke real.

La persistencia JSON sigue siendo deliberadamente local y de una sola instancia. Las nuevas consultas todavía son proyecciones pequeñas y baratas sobre el calendario cargado; `AppointmentStore` mantiene la frontera preparada para SQLite cuando consultas selectivas, volumen, transacciones o coordinación de escritores hagan visible el beneficio.

## ¿Puedo aprender PHP desde cero?

Sí. No necesitas conocer otro lenguaje. Las primeras lecciones explican requests HTTP, variables, tipos, clases, fechas y formularios a medida que AgendaPHP los necesita.

## Tooling probado

- PHP **8.5.9** estable.
- Composer **2.x** actual.
- PHPUnit **13.x**.
- GitHub Actions sobre Ubuntu 24.04.
- Navegador moderno para usar la interfaz local.

PHP 8.6 sigue en ciclo de desarrollo durante esta versión del curso y no se utiliza como runtime de producción.

## Instalar

Desde `app/`:

```bash
composer install
```

## Build / validación

```bash
bash tools/verify.sh
```

## Test

```bash
composer test
```

## Run

```bash
composer serve
```

Abre `http://127.0.0.1:8080`. Por defecto AgendaPHP usa UTC y `data/appointments.json`. Puedes configurar `AGENDA_TIMEZONE` y `AGENDA_DATA_FILE` antes de ejecutar el servidor.

## Contenido actual — 12/17

1. [Tu primera agenda web con PHP](lessons/01-tu-primera-agenda-web.md)
2. [Tipos, clases y una cita válida](lessons/02-tipos-clases-y-citas-validas.md)
3. [Formulario, POST y persistencia JSON](lessons/03-formulario-post-y-persistencia.md)
4. [Evitar cruces de horario + Checkpoint 01](lessons/04-evitar-cruces-y-checkpoint-01.md)
5. [Encontrar y cancelar citas](lessons/05-encontrar-y-cancelar-citas.md)
6. [Editar sin saltarse las reglas](lessons/06-editar-sin-saltarse-las-reglas.md)
7. [Consultas derivadas sin segunda fuente de verdad](lessons/07-consultas-derivadas.md)
8. [Ciclo de vida + Checkpoint 02](lessons/08-ciclo-de-vida-y-checkpoint-02.md)
9. [Consultas temporales sin duplicar estado](lessons/09-consultas-temporales.md)
10. [Resumen derivado y capacidad visible](lessons/10-resumen-derivado-y-capacidad.md)
11. [Exportar CSV como frontera](lessons/11-exportar-csv-como-frontera.md)
12. [Fallos operativos + Checkpoint 03](lessons/12-fallos-operativos-y-checkpoint-03.md)

## Checkpoints

- [Checkpoint 01](exercises/checkpoint-01.md) → [solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) → [solución](solutions/checkpoint-02.md)
- [Checkpoint 03](exercises/checkpoint-03.md) → [solución](solutions/checkpoint-03.md)

## Arquitectura actual

```text
HTTP + HTML / CSV
   ↓
AppointmentService + proyecciones de Schedule
   ↓
Schedule / Appointment
   ↑
AppointmentStore
   ↑
JsonAppointmentStore
```

Create, update y cancel construyen un estado candidato y sólo lo persisten después de validar. Las consultas por fecha/servicio, el conteo y los minutos se reconstruyen desde el mismo `Schedule`; CSV sólo representa esa proyección.

## Experiencia y accesibilidad

La interfaz usa HTML nativo, labels, foco visible, mensajes textuales, reflow para pantallas estrechas y acciones con nombres explícitos. Los filtros tienen controles nativos y el resumen visible usa `aria-live="polite"`. Editar conserva valores y “Cancelar cita” usa POST. El objetivo web del curso es WCAG 2.2 AA.

## ¿Qué tipo de trabajo usa estas habilidades?

PHP se usa principalmente en aplicaciones y servicios web. Un trabajo junior puede implicar mantener formularios, validaciones, endpoints, plantillas server-side, exportaciones, persistencia, pruebas y dependencias existentes. Frameworks relevantes se abordarán cuando resuelvan un problema visible, no antes de comprender PHP y HTTP.

## FAQ

### ¿Por qué no empezar directamente con Laravel?

Porque estas lecciones hacen visibles PHP, HTTP, dominio, persistencia y representación. Un framework será útil cuando reduzca complejidad real sin ocultar los fundamentos que estamos aprendiendo.

### ¿Por qué seguimos con JSON después de agregar filtros y CSV?

Porque las consultas actuales recorren una colección pequeña en una sola instancia y no requieren joins, índices ni transacciones multiwriter. Si el volumen, consultas selectivas o coordinación de escrituras cambian esa realidad, `AppointmentStore` permite sustituir JSON por SQLite sin mover las reglas del dominio.

### ¿Por qué una fecha inválida da 422 y un JSON corrupto 503?

La fecha inválida pertenece a la petición y puede corregirla quien la envió. El JSON corrupto significa que la dependencia durable no puede entregar un estado confiable; fingir una agenda vacía sería peligroso.

### ¿La aplicación ya es segura para Internet?

No. Es una aplicación educativa local. Escapamos salida y validamos entradas, pero autenticación, autorización, CSRF, TLS, rate limiting y hardening de producción pertenecen al bloque profesional posterior.

## Glosario

- **Request:** petición HTTP enviada al servidor.
- **POST:** método usado para mutaciones en este curso.
- **Dominio:** reglas independientes de transporte y persistencia.
- **Estado candidato:** versión que se valida antes de hacerla durable.
- **Proyección:** vista calculada desde el estado autoritativo sin persistir una segunda copia.
- **Rango semiabierto:** intervalo `[inicio, fin)` que incluye el comienzo y excluye el final.
- **503:** respuesta HTTP usada aquí cuando el almacenamiento local no puede entregar o guardar estado confiable.
- **PSR-4:** convención de autoloading usada por Composer.
- **PHPDoc:** documentación estándar de APIs PHP.

## Cómo hablar de este proyecto en una entrevista

Explica el problema y luego las decisiones: intervalos sin cruces, identidad estable, estados candidatos, consultas derivadas, CSV en una frontera independiente, `AppointmentStore` y fallos operativos explícitos. Señala la limitación real de JSON single-process y explica qué señal justificaría SQLite.

Preguntas probables:

- ¿Qué aporta `declare(strict_types=1)`?
- ¿Por qué un día se consulta con `[inicio, siguiente medianoche)`?
- ¿Cómo evitas que tabla, resumen y CSV diverjan?
- ¿Por qué `fputcsv` es preferible a concatenar columnas manualmente?
- ¿Por qué una corrupción durable no se convierte en agenda vacía?
- ¿Qué falla si dos procesos escriben el mismo JSON?
- ¿Cuándo migrarías a SQLite o un framework?

## Referencias oficiales

- [PHP](https://www.php.net/)
- [PHP: Supported Versions](https://www.php.net/supported-versions.php)
- [PHP manual — Classes and Objects](https://www.php.net/manual/en/language.oop5.php)
- [PHP manual — Date and Time](https://www.php.net/manual/en/book.datetime.php)
- [PHP manual — fputcsv](https://www.php.net/manual/en/function.fputcsv.php)
- [Composer](https://getcomposer.org/)
- [PHPUnit](https://phpunit.de/)
- [HTTP Semantics](https://www.rfc-editor.org/rfc/rfc9110.html)
- [WCAG 2.2](https://www.w3.org/TR/WCAG22/)
