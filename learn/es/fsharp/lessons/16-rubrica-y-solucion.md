# Lección 16 — Rúbrica y solución de referencia

## Qué vas a conseguir

Vas a evaluar tu implementación con criterios observables y comparar decisiones sin convertir la solución de referencia en la única respuesta válida.

## Rúbrica de 100 puntos

### 1. Lectura y localización — 15 puntos
- 15: identifica correctamente las fronteras de entrada, dominio, pricing, reporting y persistencia.
- 8: encuentra la mayor parte, pero mezcla alguna responsabilidad.
- 0: modifica módulos al azar sin poder explicar el flujo.

### 2. Modelado de la referencia — 20 puntos
- 20: el estado inválido se evita o rechaza explícitamente y la abstracción tiene un dueño claro.
- 10: funciona, pero depende de strings sin contrato o valida demasiado tarde.
- 0: el nuevo dato contamina reglas de precios o genera estados ambiguos.

### 3. Manejo de errores — 15 puntos
- 15: el failure mode elegido produce un `Error` útil antes de persistir.
- 8: el error existe, pero se detecta tarde o pierde contexto.
- 0: se oculta el fallo o se convierte en éxito silencioso.

### 4. Pruebas — 20 puntos
- 20: protege caso feliz y failure mode con tests centrados en comportamiento.
- 10: sólo protege uno o depende demasiado de detalles internos.
- 0: no existe evidencia automatizada nueva.

### 5. Bugfix y depuración — 10 puntos
- 10: demuestra reproducción, causa y corrección del defecto controlado.
- 5: corrige, pero no explica cómo aisló la causa.
- 0: no hay evidencia de depuración.

### 6. Documentación y diseño — 10 puntos
- 10: usa documentación oficial y propone una mejora coherente con las responsabilidades actuales.
- 5: cumple sólo una de las dos partes.
- 0: no puede justificar decisiones ni fuente consultada.

### 7. Calidad integral — 10 puntos
- 10: build/tests pasan, el reporte sigue determinista y las reglas monetarias no cambian accidentalmente.
- 5: hay deuda menor explicada que no rompe el contrato principal.
- 0: existe una regresión conocida o el proyecto no construye.

## Interpretación

- **0–59:** aún necesitas práctica guiada sobre el mismo proyecto.
- **60–79:** ya puedes resolver varios cambios acotados, pero conviene supervisión cercana.
- **80–100:** tienes evidencia razonable para intentar tareas junior con supervisión y explicar el proyecto con seguridad razonable.

La puntuación no promete empleo ni reemplaza una entrevista técnica real.

## Solución de referencia

Sólo después de tu intento, abre:

[Ver solución de referencia](../solutions/evaluacion-final-referencia.md)

La referencia muestra una opción: modelar un valor opcional validado cerca de la frontera de entrada, conservar `Pricing` independiente y extender únicamente la representación de salida. Otra solución puede ser igualmente válida si mantiene los contratos.

## Revisión comparativa

Compara tu implementación con estas preguntas:

- ¿El dato nuevo puede existir inválido dentro del dominio?
- ¿La regla monetaria conoce algo que no debería conocer?
- ¿El caso sin referencia conserva el comportamiento anterior?
- ¿La salida es reproducible para la misma entrada?
- ¿Tus tests fallarían si alguien eliminara la validación?

## Siguiente paso

La última lección convierte el proyecto en una historia técnica defendible y deja rutas oficiales para continuar aprendiendo F# y .NET.

[Anterior](15-evaluacion-final.md) · [Siguiente](17-entrevista-y-siguiente-paso.md)
