# Lección 11 — Seguridad web y límites de entrada

## Qué vas a conseguir

Reducirás la superficie de riesgo de la importación sin convertir ContactDesk en un curso completo de ciberseguridad.

## El problema

Un formulario de archivos acepta bytes controlados por otra persona. Aunque ContactDesk sea local, no debemos asumir que todo archivo es pequeño, correcto o inocuo.

## Concepto

La seguridad útil empieza con límites concretos:

- la importación usa `POST`, no una operación de lectura;
- Rails conserva su protección CSRF normal;
- el controlador exige un objeto que pueda leerse;
- `ContactTransfer` sólo acepta hasta `MAX_BYTES`;
- se exige el conjunto completo de encabezados;
- cada fila atraviesa las validaciones de `Contact`;
- al navegador sólo vuelve `ImportError`, no detalles arbitrarios de excepciones internas.

`accept=".csv,text/csv"` mejora la experiencia del selector, pero **no es una frontera de seguridad**: el servidor sigue validando el contenido.

## Código real

- Rutas: [`../app/config/routes.rb`](../app/config/routes.rb)
- Vista: [`../app/app/views/contacts/index.html.erb`](../app/app/views/contacts/index.html.erb)
- Límite y parser: [`../app/app/services/contact_transfer.rb`](../app/app/services/contact_transfer.rb)

## Tu turno

Escribe una prueba que envíe más de `ContactTransfer::MAX_BYTES`. Debe fallar de forma explícita y no persistir ningún contacto.

## Errores comunes

- confiar en `Content-Type` o en `.csv`;
- quitar CSRF porque dificulta una prueba;
- permitir archivos sin límite;
- incluir SQL, rutas o stack traces en mensajes dirigidos al usuario.

## Siguiente paso

En la lección 12 usaremos pruebas, logs y reproducción mínima para diagnosticar fallos sin adivinar.

## Referencias

- https://guides.rubyonrails.org/security.html
- https://guides.rubyonrails.org/action_controller_overview.html
