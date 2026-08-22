# Lección 04 — Errores explícitos y pruebas que no dependen de tu PC

## Qué vas a conseguir

Vas a hacer que WorkstationAudit falle con contexto útil y que sus reglas puedan probarse sin llenar tu disco a propósito.

## Antes de empezar

Debes haber completado las lecciones 01–03 y poder ejecutar el auditor.

## El problema

Una herramienta de diagnóstico deja de ser confiable si:

- oculta un error de lectura;
- depende de que el equipo de CI tenga cierto porcentaje de disco libre;
- confunde “no tengo datos” con “todo está bien”.

## Concepto

PowerShell distingue errores terminantes y no terminantes. Para una frontera crítica de recolección, WorkstationAudit convierte el fallo en una excepción contextual con `try/catch`.

Para las reglas, Pester usa snapshots artificiales. Eso permite comprobar 5%, 15%, 25% o capacidad cero sin depender del hardware real.

## Demostración

[DEMO] Ejecuta las pruebas:

```powershell
Invoke-Pester -Path ./app/tests -Output Detailed
```

Después inspecciona un caso:

```powershell
$drive = [pscustomobject]@{
    Name = 'Fixture'
    FreeBytes = 5
    TotalBytes = 100
}

$drive | Get-StorageFinding
```

La severidad debe ser `Critical`.

## Código real

Ver:

- `../app/WorkstationAudit.psm1`
- `../app/tests/WorkstationAudit.Tests.ps1`

## Qué acaba de pasar

El hardware es I/O y queda en un borde. La regla recibe objetos. Esa separación permite pruebas deterministas y evita mocks gigantes.

## Errores comunes

- usar `-ErrorAction SilentlyContinue` como estrategia general;
- devolver `$null` ante un fallo sin distinguirlo de ausencia legítima;
- hacer tests que dependen del espacio libre real;
- afirmar que una prueba en un runner prueba todos los equipos Windows existentes.

## Buenas prácticas

- captura sólo donde puedas añadir contexto o recuperar;
- conserva la excepción interna cuando sea útil;
- prueba decisiones, límites y failure modes;
- usa fixtures pequeños y legibles.

## Tu turno

Completa el checkpoint y añade una prueba para el límite exacto de 10%.

## Cómo comprobar tu solución

```powershell
Invoke-Pester -Path ./app/tests -CI
```

## Solución

La solución de referencia del checkpoint está separada en `../solutions/checkpoint-01.md`; no la abras antes de completar el ejercicio.

## Reto adicional

Añade una prueba para exactamente 10% libre y decide, leyendo la implementación, si el contrato actual produce `Warning` o `Critical`.

## Resumen

WorkstationAudit ya tiene errores explícitos, pruebas deterministas y un smoke path contra el sistema real.

## Siguiente paso

Completa [Checkpoint 01 — Añade una regla diagnóstica](../exercises/checkpoint-01.md) y continúa con [Lección 05 — Configura reglas sin editar código](05-configura-reglas-sin-editar-codigo.md).

## Referencias

- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_try_catch_finally
- https://pester.dev/docs/quick-start
