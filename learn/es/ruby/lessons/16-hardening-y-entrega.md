# Lección 16 — Hardening: listo para operar no significa invulnerable

## Qué vas a conseguir

Harás una revisión de entrega de ContactDesk: configuración por entorno, secretos fuera del repositorio, migraciones, health, logs, límites de entrada y pruebas repetibles.

## El problema

Una app que pasa tests puede fallar en producción por configuración, permisos, base no migrada o secretos mal gestionados. Tampoco debemos prometer “seguridad completa” porque exista CSRF o un health check.

## Checklist operativo

Antes de desplegar:

1. ejecuta `bin/rails db:prepare` en el entorno correcto;
2. ejecuta `bin/rails test` y el smoke;
3. conserva secretos/credenciales fuera del repositorio;
4. usa HTTPS en el terminador/proxy apropiado;
5. comprueba `/healthz` sin exponer información sensible;
6. revisa logs usando request IDs;
7. mantén límites explícitos para importación y paginación;
8. define backup/restore de la base para el entorno real;
9. documenta quién puede ejecutar tareas operativas;
10. no mezcles diagnóstico con remediación automática sin una historia y pruebas específicas.

## Código real

- [`../app/config/environments/production.rb`](../app/config/environments/production.rb)
- [`../app/config/routes.rb`](../app/config/routes.rb)
- [`../app/script/smoke.rb`](../app/script/smoke.rb)

[EJECUTAR]

```bash
bin/rails test
RAILS_ENV=test bin/rails runner script/smoke.rb
bin/rails contactdesk:diagnostics
```

## Tu turno

Elige tres puntos del checklist y explica qué evidencia observable usarías para afirmar que están satisfechos. Evita respuestas como “porque Rails lo hace”.

## Checkpoint

Completa [Checkpoint 04 — prepara ContactDesk para operación](../exercises/checkpoint-04.md).

## Siguiente paso

Continúa con la [Lección 17 — Evaluación final: entrega ContactDesk](17-evaluacion-final.md). Leerás código existente, corregirás un bug, añadirás una capacidad y defenderás tus decisiones sin receta paso a paso.

## Referencias

- https://guides.rubyonrails.org/configuring.html
- https://guides.rubyonrails.org/security.html
- https://guides.rubyonrails.org/command_line.html
