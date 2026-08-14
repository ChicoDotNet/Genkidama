# Rúbrica final — BackupForge

Puntaje total: **100 puntos**. Evalúa comportamiento y explicación, no similitud textual con la solución de referencia.

## 1. Lectura y arquitectura — 15 puntos

- **5**: identifica correctamente biblioteca/core, CLI y fronteras de filesystem.
- **5**: conserva `Result` y evita mezclar `process::exit`/stdout con la API pública.
- **5**: explica con claridad por qué `verify`, `audit`, restore y snapshots tienen contratos distintos.

## 2. Restauración selectiva — 20 puntos

- **8**: restaura exactamente una ruta declarada conservando estructura relativa.
- **6**: rechaza una ruta inexistente/no declarada o insegura con error explícito.
- **6**: no publica el archivo restaurado cuando la evidencia de integridad requerida falla.

## 3. Bug de rutas equivalentes — 15 puntos

- **8**: una regresión demuestra el problema con un componente `.` u otra forma equivalente razonable.
- **7**: la corrección vive en validación compartida del manifest y no sólo en una ruta de CLI.

## 4. Pruebas, errores y tooling — 20 puntos

- **8**: agrega pruebas deterministas/offline para la funcionalidad y el bugfix.
- **4**: errores recuperables siguen siendo `Result`/`BackupError` con diagnóstico útil.
- **4**: API pública nueva tiene rustdoc suficiente para uso correcto.
- **4**: `bash tools/verify.sh` termina verde sin relajar gates.

## 5. Documentación oficial — 10 puntos

- **5**: consulta al menos dos fuentes oficiales pertinentes.
- **5**: conecta cada fuente con una decisión concreta del cambio, no sólo pega enlaces.

## 6. Diseño de siguiente paso — 10 puntos

- **4**: identifica una frontera razonable para almacenamiento remoto.
- **3**: trata publicación parcial, colisiones/escrituras perdidas y coordinación multi-writer.
- **3**: distingue checksum de autenticidad y propone observabilidad respetuosa de datos sensibles.

## 7. Defensa de entrevista — 10 puntos

- **4**: explica el problema y arquitectura sin narrar archivos línea por línea.
- **3**: reconoce al menos dos límites reales del sistema.
- **3**: defiende un tradeoff concreto y qué evidencia usaría para reconsiderarlo.

## Interpretación

- **90–100:** evidencia sólida de preparación inicial para trabajar sobre una base Rust pequeña con supervisión razonable.
- **75–89:** buen fundamento; conviene reforzar las áreas perdidas antes de presentar el proyecto.
- **60–74:** comprensión parcial; repite las lecciones/checkpoints relacionadas con los criterios débiles.
- **<60:** todavía falta práctica autónoma antes de considerar completado el objetivo del curso.

Una puntuación alta **no garantiza empleo**. La rúbrica mide la evidencia producida en este proyecto educativo, no desempeño futuro en cualquier equipo o dominio.
