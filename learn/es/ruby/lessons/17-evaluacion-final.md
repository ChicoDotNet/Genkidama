# Lección 17 — Evaluación final: entrega ContactDesk

## Qué vas a conseguir

Demostrar que puedes leer, modificar, probar y explicar una aplicación Ruby on Rails existente sin seguir una receta paso a paso.

## Antes de empezar

Completa las lecciones 1–16 y los cuatro checkpoints. Esta evaluación no introduce una biblioteca nueva: combina Ruby, Rails, Active Record, HTTP, validaciones, asociaciones, CSV, transacciones, paginación, observabilidad y operación segura.

## El problema

Una base Junior útil no consiste en recordar generadores de Rails. Necesitas entrar a código existente, localizar responsabilidades, corregir un defecto, añadir una capacidad, protegerla con pruebas y explicar por qué elegiste una solución proporcionada.

## Encargo final

Trabaja primero con el [ejercicio final versionado](../exercises/final-contactdesk.md). No abras la solución de referencia hasta haber intentado las seis historias.

### Historia 1 — Seguimiento vencido

Añade a los contactos una fecha opcional de próximo seguimiento y una consulta que permita encontrar seguimientos vencidos. La regla debe vivir en una frontera que pueda probarse sin depender de la hora exacta del runner.

### Historia 2 — Corrige un defecto de búsqueda

Supón que un usuario busca un email con espacios al principio o al final y ContactDesk no devuelve el contacto existente. Corrige el defecto sin duplicar normalización en controlador y modelo, y añade una prueba de regresión.

### Historia 3 — Importación con evidencia útil

Cuando una importación CSV falla, mejora el error para identificar la fila lógica que causó el problema sin incluir nombre, email, notas u otro dato sensible en logs o mensajes operativos. Conserva el rollback transaccional.

### Historia 4 — Prueba una modificación HTTP

Expón el filtro de seguimiento vencido desde la pantalla de contactos y añade una prueba de integración que demuestre el comportamiento. No basta con probar únicamente el scope de Active Record.

### Historia 5 — Diagnóstico operativo

Extiende `Contactdesk::Diagnostics` para informar cuántos seguimientos están vencidos, sin listar contactos ni PII. Conserva `/healthz` pequeño y apropiado para health checks: no conviertas ese endpoint en un dashboard.

### Historia 6 — Evolución de diseño

Imagina que ContactDesk necesita enviar recordatorios por email en el futuro. Diseña la frontera mínima para preparar esa capacidad sin enviar correo todavía y sin meter lógica de entrega dentro del modelo `Contact`. Explica qué dejarías fuera del alcance y por qué.

## Evidencia obligatoria

Tu entrega debe incluir:

1. código Ruby/Rails modificado con nombres y responsabilidades claras;
2. una prueba de regresión para el bugfix;
3. una prueba de integración para la nueva capacidad visible;
4. manejo explícito del error de importación sin filtrar PII;
5. `bin/rails test` y el smoke verdes;
6. una referencia concreta a documentación oficial consultada;
7. una breve nota de diseño sobre la capacidad que deliberadamente no implementaste.

## Cómo comprobar

Desde `learn/es/ruby/app`:

```bash
bundle install
bin/rails db:prepare
bin/rails test
RAILS_ENV=test bin/rails runner script/smoke.rb
bin/rails contactdesk:diagnostics
```

Si cambias migraciones, demuestra que una base limpia puede prepararse y que las pruebas no dependen del contenido de tu base de desarrollo.

La política del repositorio no exige 100% de code coverage. Cuando exista una medición razonable, 44% es piso suficiente si contratos, failure modes y regresiones relevantes están protegidos; 44%–72.8% es plenamente aceptable y una cifra superior es bienvenida.

## Rúbrica — 100 puntos

| Área | Puntos | Evidencia esperada |
|---|---:|---|
| Ruby idiomático y responsabilidades | 20 | métodos pequeños, objetos claros, scopes/servicios con propósito |
| Active Record y datos | 15 | migración/consulta coherente, validación y normalización sin duplicación |
| HTTP y comportamiento visible | 15 | ruta/controlador/vista con prueba de integración útil |
| Errores y seguridad | 15 | errores explícitos, transacción conservada y ausencia de PII innecesaria |
| Pruebas y regresión | 20 | tests que protegen bugfix y nueva funcionalidad, no sólo happy paths triviales |
| Operación y observabilidad | 5 | diagnóstico útil y health check con fronteras correctas |
| Explicación profesional | 10 | documentación consultada, trade-offs y alcance no implementado defendibles |

### Interpretación

- **85–100:** evidencia sólida de preparación Junior/Entry Level para este alcance.
- **70–84:** base razonable; repasa las áreas de menor puntuación antes de presentar el proyecto.
- **<70:** vuelve al checkpoint relacionado y repite la historia que expuso la brecha.

La rúbrica mide tu capacidad sobre ContactDesk; no promete empleo.

## Cómo hablar de este proyecto en una entrevista

Prepárate para responder señalando código y pruebas reales:

1. ¿Por qué ContactDesk usa scopes/consultas antes de paginar?
2. ¿Qué diferencia hay entre validación de modelo e índice único de base de datos?
3. ¿Por qué la importación CSV es transaccional?
4. ¿Qué aprendiste al hacer funcionar el mismo proyecto en Ubuntu y Windows?
5. ¿Por qué `Win32_Product` no tiene relación con este curso y qué te enseña eso sobre elegir APIs con efectos secundarios? No necesitas conocer PowerShell para responder: piensa en el principio de observar sin mutar accidentalmente.
6. ¿Qué información deliberadamente no expones en `/healthz` y diagnósticos?
7. ¿Qué aporta un `request_id` al debugging?
8. ¿Qué parte de ContactDesk moverías primero si creciera la lógica de negocio?
9. ¿Qué automatizarías antes de desplegar a producción y qué seguiría siendo responsabilidad del entorno?

No memorices frases. Explica problema → decisión → evidencia → trade-off.

## Solución de referencia

Sólo después de intentar el ejercicio, compara tus decisiones con la [solución de referencia](../solutions/final-contactdesk.md). No necesitas coincidir exactamente si conservas los contratos, pruebas el comportamiento y puedes explicar tus decisiones.

## Referencias

- https://www.ruby-lang.org/en/documentation/
- https://guides.rubyonrails.org/active_record_basics.html
- https://guides.rubyonrails.org/testing.html
- https://guides.rubyonrails.org/security.html
- https://api.rubyonrails.org/

## Siguiente paso

Si tu entrega satisface la rúbrica y los gates, conserva ContactDesk como proyecto de práctica y sigue ejercitando cambios pequeños sobre código existente. Para ramas, historia y colaboración utiliza el [curso transversal de Git](../../git/README.md).
