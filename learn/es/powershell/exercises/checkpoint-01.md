# Checkpoint 01 — Añade una regla diagnóstica

## Objetivo

Demostrar que puedes extender WorkstationAudit sin una receta paso a paso.

## Encargo

Añade una función `Get-PowerShellVersionFinding` que reciba un snapshot y produzca un hallazgo estructurado sobre la versión mayor de PowerShell.

Requisitos observables:

- recibe el snapshot como dato; no consulta `$PSVersionTable` internamente;
- para PowerShell 7 o posterior produce severidad `Info`;
- para una versión mayor menor que 7 produce `Warning` y un mensaje que recomiende PowerShell moderno sin afirmar que el equipo es inseguro;
- conserva `Code`, `Severity`, `Message` y `Evidence`;
- añade al menos dos pruebas Pester;
- integra el hallazgo en `Get-WorkstationAudit`.

## Comprobación

```powershell
Invoke-Pester ./app/tests -Output Detailed
./app/Invoke-Audit.ps1 -OutputPath ./audit.json
```

Las pruebas deben quedar verdes y el JSON debe incluir el nuevo hallazgo.

## Reflexión

¿Por qué la regla recibe un snapshot en vez de consultar directamente la sesión? ¿Qué gana la prueba y qué coste añade esa frontera?

## Solución

Inténtalo primero. Después consulta [la solución de referencia](../solutions/checkpoint-01.md).

## Siguiente paso

Cuando puedas explicar tu implementación, continúa con la futura lección 05 sobre consultas de sistema y fronteras específicas de Windows.
