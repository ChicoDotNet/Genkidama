# Rúbrica final — PHP / AgendaPHP

Puntaje total: **100 puntos**. Evalúa comportamiento y criterio, no similitud línea por línea con la solución de referencia.

## 1. Lectura y arquitectura — 15 puntos

- 5: distingue dominio, aplicación, persistencia y frontera HTTP/representación.
- 5: coloca el nuevo estado sin crear una segunda fuente de verdad ni acoplar dominio a HTTP/JSON.
- 5: explica al menos un trade-off real de la arquitectura actual.

## 2. Funcionalidad nueva — 20 puntos

- 6: una cita nueva comienza pendiente.
- 6: confirmar es una mutación explícita y durable.
- 4: tabla/CSV leen el mismo estado autoritativo.
- 4: JSON legado sin estado conserva compatibilidad mediante un default documentado.

## 3. Bugfix de normalización — 15 puntos

- 7: rechaza entrada visualmente vacía que el comportamiento anterior aceptaba.
- 4: la política es pequeña, explícita y no destruye nombres válidos.
- 4: existe una regresión que demuestra el defecto corregido.

## 4. Errores, consistencia y seguridad HTTP — 15 puntos

- 5: una falla durable no publica estado fantasma ni se interpreta como agenda vacía.
- 5: la nueva mutación conserva CSRF, media type y límite de body antes de tocar estado.
- 5: distingue correctamente errores de entrada/operación y no exagera el nivel de seguridad alcanzado.

## 5. Pruebas, tooling y coverage — 15 puntos

- 5: `tools/verify.sh` y `tools/smoke.sh` pasan.
- 5: las pruebas protegen comportamiento/regresiones reales y son deterministas.
- 5: cuando code coverage sea medible, alcanza al menos **44%** con foco en contratos relevantes; 44%–72.8% es plenamente suficiente y una cifra mayor no es requisito.

## 6. Documentación y criterio profesional — 10 puntos

- 4: consulta y aplica al menos dos fuentes oficiales/primarias.
- 3: contratos nuevos o modificados están explicados donde corresponde.
- 3: no hace claims de seguridad, escalabilidad o producción superiores a la evidencia.

## 7. Diseño de siguiente escala — 10 puntos

- 4: identifica `AppointmentStore` como frontera natural de almacenamiento y plantea migración de datos.
- 3: considera coordinación/concurrencia y evita lost updates.
- 3: compara SQLite/PDO y framework desde presiones reales, incluyendo observabilidad/seguridad.

## Interpretación

- **90–100:** evidencia fuerte para defender AgendaPHP como proyecto Junior/Entry Level.
- **75–89:** base razonable; repasa los criterios donde perdiste puntos antes de presentar el proyecto.
- **60–74:** comprendiste partes relevantes, pero conviene practicar mantenimiento autónomo.
- **<60:** vuelve a checkpoints y repite la evaluación sin copiar la solución.

Una puntuación alta demuestra dominio de esta evaluación; **no garantiza empleo ni sustituye experiencia profesional**.
