# Rúbrica final — Java / HelpDesk API

Puntaje total: **100**. Una referencia razonable de dominio inicial es 70/100, siempre que no exista una falla crítica de integridad. La puntuación no equivale a promesa de empleo.

## 1. Funcionalidad y modelado — 25 puntos

- 10: responsable opcional, normalizado y validado sin acoplar dominio a HTTP.
- 8: asignar y retirar responsable funciona y persiste.
- 7: consulta pendiente filtra y ordena determinísticamente.

## 2. Integridad, errores y compatibilidad — 20 puntos

- 8: JSON legacy sin responsable sigue cargando.
- 7: restauración rechaza duplicados/estado inválido sin publicar parcialmente.
- 5: errores externos son explícitos y útiles.

Una restauración que acepta silenciosamente IDs duplicados limita esta sección a 5/20.

## 3. Pruebas y regresión — 20 puntos

- 8: pruebas de responsable válido, inválido y compatibilidad legacy.
- 6: prueba determinista del orden de pendientes.
- 6: regresión de persistencia/integridad y suite existente verde.

## 4. Calidad Java y arquitectura — 15 puntos

- 5: tipos/nombres claros y Javadoc donde la API pública lo requiere.
- 5: reglas deterministas separadas de HTTP, archivos y Jackson.
- 5: no introduce dependencias o abstracciones sin una necesidad demostrable.

## 5. Tooling, documentación y diagnóstico — 10 puntos

- 4: `mvn verify` y ejecución documentada/reproducible.
- 3: dos fuentes oficiales realmente usadas para decidir.
- 3: describe un fallo real y la evidencia con la que lo diagnosticó.

## 6. Defensa profesional — 10 puntos

La respuesta explica con precisión límites de `synchronized`, persistencia JSON, medición, privacidad y una ruta razonable hacia base de datos/framework sin afirmar capacidades inexistentes.

## Fallas críticas

No se considera aprobada la evaluación si la entrega desactiva pruebas para obtener verde, pierde silenciosamente tickets, publica memoria antes de una persistencia fallida, introduce secretos reales o afirma seguridad/consistencia distribuida que el sistema no posee.

## Interpretación

- **90–100:** evidencia Junior especialmente sólida para este alcance.
- **80–89:** buen dominio inicial con detalles menores por pulir.
- **70–79:** cumple el objetivo mínimo, con áreas concretas para practicar.
- **<70:** conviene repetir las historias débiles y volver a defender las decisiones.

Usa esta rúbrica para orientar práctica y conversación técnica, no para inferir contratación ni salario.
