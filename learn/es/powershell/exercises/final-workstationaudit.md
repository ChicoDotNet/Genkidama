# Evaluación final — WorkstationAudit

Resuelve este reto sin instrucciones paso a paso.

## Escenario

Un equipo de soporte quiere incorporar WorkstationAudit a una rutina operativa. Antes de hacerlo necesita una nueva señal diagnóstica sencilla, un contrato más resistente ante datos incompletos y evidencia automatizada de que el cambio no rompe reportes existentes.

## Historias

### 1. Comprende antes de cambiar
Dibuja o describe en máximo diez líneas el flujo desde recolección hasta reporte. Identifica al menos dos fronteras que evitan efectos secundarios.

### 2. Añade una señal funcional
Agrega **una** regla diagnóstica nueva basada en una señal observable del equipo o de un fixture. Debe producir un finding estructurado con `Code`, `Severity`, `Message` y `Evidence`.

Restricción: no conviertas la auditoría en remediación.

### 3. Corrige un failure mode
Introduce un fixture incompleto o inesperado que hoy pueda producir un resultado ambiguo. Haz que el comportamiento sea explícito: resultado seguro o excepción contextual, según corresponda.

### 4. Protege el cambio
Añade al menos una prueba Pester que habría fallado antes de tu implementación y ahora pase. Mantén verde la suite existente.

### 5. Consulta una fuente oficial
Incluye en tu entrega un enlace a documentación oficial que haya influido en una decisión concreta y explica en una frase qué verificaste allí.

### 6. Diseña la siguiente mejora
Propón una mejora futura y responde: valor, principal riesgo, cómo medirías éxito y qué NO implementarías todavía.

## Evidencia mínima

- `Invoke-Pester ./app/tests` verde.
- Smoke de `Invoke-Audit.ps1` generando JSON válido.
- Diff pequeño y explicable.
- Sin secretos ni cambios silenciosos de configuración del sistema.

## Rúbrica — 100 puntos

| Área | Puntos |
|---|---:|
| Comprensión del código y arquitectura | 15 |
| Funcionalidad nueva útil y acotada | 20 |
| Manejo explícito del failure mode | 15 |
| Prueba automatizada relevante | 20 |
| Calidad/idioms/legibilidad | 10 |
| Uso correcto de documentación oficial | 5 |
| Seguridad y efectos secundarios | 10 |
| Explicación de mejora futura y trade-offs | 5 |

### Interpretación

- **0–59:** aún necesitas práctica guiada en varias competencias del curso.
- **60–79:** puedes resolver tareas acotadas con revisión y supervisión.
- **80–100:** demuestras una base Junior razonable sobre este proyecto y puedes explicar tus decisiones con claridad.

La puntuación no promete empleo ni sustituye experiencia profesional.

Cuando termines, consulta [la solución de referencia](../solutions/final-workstationaudit.md) sólo para comparar enfoque y trade-offs.
