# Evaluación final — ContactDesk

Esta evaluación se resuelve sobre la aplicación real. No hay receta paso a paso. Antes de consultar la solución de referencia, completa las seis historias y deja evidencia reproducible.

## Reglas

- Conserva Rails 8.1 / Ruby 4.0 y las dependencias actuales salvo necesidad justificada.
- No desactives validaciones, CSRF, transacciones, tests ni checks para obtener verde.
- No agregues datos reales ni secretos a fixtures, logs o documentación.
- Mantén el cambio acotado: una historia nueva no justifica reescribir ContactDesk.

## Historias

1. **Seguimiento vencido.** Añade una fecha opcional de próximo seguimiento y permite localizar contactos vencidos respecto de una fecha de referencia controlable en tests.
2. **Bugfix de búsqueda.** Una búsqueda de email con espacios externos debe encontrar el mismo contacto que el email normalizado. Añade regresión.
3. **Error CSV útil.** Si una fila importada es inválida, informa su número lógico sin incluir PII. Ningún registro de esa importación debe persistir.
4. **Filtro visible.** Expón el filtro de seguimiento vencido en la interfaz y pruébalo mediante request/integration test.
5. **Diagnóstico sin PII.** Añade al diagnóstico agregado el número de seguimientos vencidos, sin listar identidades ni convertir `/healthz` en reporte detallado.
6. **Diseño futuro.** Describe una frontera para recordatorios por email sin implementar el envío ni colocarlo dentro del modelo `Contact`.

## Evidencia mínima

Entrega:

- migración y código de aplicación que implementen las historias aplicables;
- pruebas nuevas para bugfix, consulta, importación y HTTP;
- salida verde de `bin/rails test`;
- smoke verde;
- una referencia oficial que hayas consultado;
- una nota breve de diseño con al menos un trade-off y una cosa deliberadamente fuera de alcance.

## Resultado esperado

Un revisor debe poder ejecutar desde `app/`:

```bash
bundle install
bin/rails db:prepare
bin/rails test
RAILS_ENV=test bin/rails runner script/smoke.rb
bin/rails contactdesk:diagnostics
```

y obtener una aplicación coherente, sin depender de tu base local ni de datos personales.

## Antes de ver la referencia

Comprueba tu trabajo con la rúbrica de la [Lección 17](../lessons/17-evaluacion-final.md). Después puedes comparar con [`../solutions/final-contactdesk.md`](../solutions/final-contactdesk.md).
