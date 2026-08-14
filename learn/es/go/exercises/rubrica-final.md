# Rúbrica final — Go / UptimeLab

Puntaje total: **100 puntos**. La rúbrica evalúa comportamiento y criterio, no similitud línea por línea con la solución de referencia.

## 1. Lectura y arquitectura — 15 puntos

- 5: identifica correctamente responsabilidades de `monitor`, `history`, `insights`, `scheduler`, `web` y composición.
- 5: coloca el cambio de target habilitado/deshabilitado en una frontera coherente sin contaminar paquetes no relacionados.
- 5: explica al menos un tradeoff real de la arquitectura actual.

## 2. Funcionalidad nueva — 20 puntos

- 8: un target deshabilitado no realiza requests.
- 6: no contribuye a summary/trends mientras está deshabilitado.
- 6: configuración legacy sigue funcionando o existe una migración/normalización explícita y segura.

## 3. Bugfix de integridad — 15 puntos

- 8: rechaza nombres duplicados después de la normalización elegida.
- 4: el error es explícito y accionable.
- 3: existe regresión que demuestra el defecto corregido.

## 4. Errores, consistencia y cancelación — 15 puntos

- 5: conserva la diferencia transporte vs respuesta HTTP.
- 5: no introduce estado fantasma ante fallas.
- 5: cancelación/errores se propagan idiomáticamente y están protegidos por prueba cuando aplica.

## 5. Pruebas y tooling — 15 puntos

- 5: `gofmt`, `go vet` y build pasan.
- 5: `go test -race ./...` pasa.
- 5: las pruebas son offline, deterministas y detectan defectos reales en vez de afirmar detalles triviales.

## 6. Documentación y criterio profesional — 10 puntos

- 4: consulta y aplica al menos dos fuentes oficiales.
- 3: APIs exportadas nuevas o modificadas tienen Go doc útil.
- 3: no hace claims de seguridad, SLA o escalabilidad superiores a la evidencia.

## 7. Diseño de siguiente escala — 10 puntos

- 4: identifica correctamente la frontera de almacenamiento a sustituir para multi-instancia.
- 3: considera concurrencia/conflictos de escritura y consistencia.
- 3: considera observabilidad y privacidad/seguridad adicionales.

## Interpretación

- **90–100:** evidencia fuerte para defender el proyecto como candidato junior/entry-level.
- **75–89:** base razonable; repasa los criterios donde perdiste puntos antes de presentar el proyecto.
- **60–74:** comprendiste partes relevantes, pero todavía conviene practicar mantenimiento autónomo.
- **<60:** vuelve a checkpoints y repite la evaluación sin copiar la solución.

Una puntuación alta demuestra dominio de esta evaluación; **no garantiza empleo ni sustituye experiencia profesional**.