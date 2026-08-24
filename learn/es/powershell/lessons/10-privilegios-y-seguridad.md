# Lección 10 — Privilegios y seguridad de ejecución

## Qué vas a conseguir
Harás visible el contexto de privilegios del auditor y separarás diagnóstico de remediación.

## El problema
Un script que «funciona como administrador» no es automáticamente mejor. Ejecutar con privilegios amplios aumenta el impacto de un error. Además, Execution Policy no debe confundirse con una frontera de seguridad completa.

## Concepto
`Get-ExecutionContextSnapshot` observa el usuario, si el token Windows está elevado y las políticas visibles. `Get-PrivilegeFinding` convierte esa observación en un hallazgo explicable. Fuera de Windows declara que la comprobación de token no está soportada por esta implementación.

```powershell
$context = Get-ExecutionContextSnapshot
$context
Get-PrivilegeFinding -ExecutionContext $context
```

Si la sesión está elevada, WorkstationAudit genera `execution.elevated` como `Warning`. Eso no significa «tu PC está comprometida»; significa que la herramienta tiene más capacidad de modificar estado de la necesaria para una auditoría read-only.

## Principio de mínimo privilegio
Para recopilar señales normales, empieza sin elevación. Si una futura señal exige privilegios, documenta exactamente cuál, por qué y qué ocurre si falta. No agregues `Start-Process -Verb RunAs` automáticamente.

## Execution Policy
PowerShell documenta Execution Policy como una característica para controlar condiciones de carga/ejecución, no como un sistema de seguridad que impida por sí solo código malicioso. No «arregles» un equipo ejecutando `Set-ExecutionPolicy Unrestricted` desde el auditor.

## Errores comunes
- Pedir administrador por comodidad.
- Silenciar `AccessDenied` y continuar como si la señal existiera.
- Confundir Execution Policy con autorización.
- Mezclar `Set-Service`, cambios de Registro o instalaciones con una operación de lectura.

## Tu turno
Inyecta en una prueba un contexto elevado y verifica que obtienes un Warning sin tocar privilegios reales del runner.

## Siguiente paso
Continúa con [Lección 11 — Reportes para humanos y máquinas](11-reportes-humanos-y-json.md).

## Referencias
- https://learn.microsoft.com/powershell/module/microsoft.powershell.security/get-executionpolicy
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_execution_policies
