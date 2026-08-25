# Evaluación final — QuoteDesk

Resuelve este encargo sin una receta de archivos o funciones. Puedes consultar las lecciones y documentación oficial, pero no abras la solución hasta terminar un intento serio.

## Historia A — Vigencia de una cotización

El negocio necesita una fecha opcional de vigencia para las cotizaciones. Una cotización vencida no debe poder aprobarse. La información debe sobrevivir un round-trip de persistencia sin romper archivos existentes que no tengan esa propiedad.

Añade pruebas antes o junto con la implementación.

## Historia B — Regresión de integridad

Protege explícitamente que una factura no pueda construirse ni cargarse con totales que contradigan sus partidas. Si la base ya contiene esa defensa, demuestra con una regresión por qué existe y explica qué capa conserva cada contrato.

## Historia C — Error operativo

Haz que una falla de backup o de lectura continúe siendo visible para el caller. No conviertas una excepción en una lista vacía ni borres el archivo problemático. Demuestra al menos un failure mode con una prueba nueva relacionada con tu cambio.

## Historia D — UI y arquitectura

Expón la nueva fecha de vigencia en la aplicación Windows sin mover la regla “una cotización vencida no se aprueba” a un event handler del formulario. Explica dónde vive la regla y por qué.

## Historia E — Documentación oficial

Consulta al menos dos referencias oficiales de Microsoft. Una debe sustentar una decisión de VB/.NET/testing; otra debe relacionarse con WinForms, serialización o deployment. Registra brevemente qué decisión respaldó cada fuente.

## Historia F — Evolución

Sin implementarlo, diseña la transición de archivos JSON locales a almacenamiento compartido para varias estaciones. Identifica frontera a sustituir, concurrencia/conflictos, identidad de documentos, migración, backup/restore, observabilidad y datos que no deberían aparecer en logs.

## Evidencia mínima

Entrega:

1. pruebas de vigencia y compatibilidad con JSON anterior;
2. una regresión de integridad o failure mode;
3. `dotnet test` verde;
4. build WinForms verde;
5. publish `win-x64` reproducible;
6. dos referencias oficiales consultadas;
7. una defensa de arquitectura de aproximadamente cinco minutos.

Autoevalúate con [rubrica-final.md](rubrica-final.md).
