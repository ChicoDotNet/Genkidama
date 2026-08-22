# Curso de PowerShell desde cero — Construye un auditor de estaciones de trabajo

PowerShell es un shell y lenguaje de automatización orientado a objetos, muy usado para administrar Windows y servicios Microsoft y también disponible en Linux y macOS. **Puedes empezar desde cero aquí**: aprenderás el lenguaje construyendo **WorkstationAudit**, un auditor local que convierte señales del equipo en hallazgos y un reporte JSON explicable.

## Qué vas a construir

WorkstationAudit inspeccionará plataforma, versión de PowerShell y almacenamiento; transformará datos en hallazgos con severidad; manejará errores de recolección y terminará evolucionando hacia un diagnóstico más completo de una estación Windows sin modificar el equipo auditado.

El curso enseña PowerShell 7 moderno. Al 22 de agosto de 2026 Microsoft identifica **PowerShell 7.6 como la línea LTS vigente**. Windows PowerShell 5.1 aparece cuando sea necesario para explicar compatibilidad, no como runtime principal del curso.

## Requisitos

- PowerShell 7.6 LTS recomendado.
- Windows 11 para la experiencia objetivo del auditor.
- Linux actual sirve para practicar las capacidades portables y las pruebas del núcleo.
- VS Code es opcional.

Comprueba tu versión:

```powershell
$PSVersionTable.PSVersion
```

## Ejecutar la aplicación

Desde esta carpeta:

```powershell
./app/Invoke-Audit.ps1 -OutputPath ./audit.json
Get-Content ./audit.json
```

## Ejecutar pruebas

```powershell
Install-Module Pester -MinimumVersion 5.7.1 -Scope CurrentUser
Invoke-Pester ./app/tests
```

Pester es la única dependencia de desarrollo del primer incremento; la aplicación usa únicamente capacidades incluidas en PowerShell/.NET.

## Avance

**4/17 lecciones.** El primer checkpoint ya produce un diagnóstico pequeño, observable y probado.

1. [Lección 01 — Ejecuta tu primer auditor](lessons/01-ejecuta-tu-primer-auditor.md)
2. [Lección 02 — Trabaja con objetos y pipeline](lessons/02-objetos-y-pipeline.md)
3. [Lección 03 — Convierte señales en hallazgos](lessons/03-funciones-parametros-y-hallazgos.md)
4. [Lección 04 — Maneja errores y prueba comportamiento](lessons/04-errores-y-pruebas.md)
5. Próximo: consultas de sistema y fronteras Windows.
6. Próximo: colecciones y reglas de auditoría.
7. Próximo: configuración del auditor.
8. Próximo: checkpoint de diagnóstico reproducible.
9. Próximo: inventario de software/servicios con límites explícitos.
10. Próximo: seguridad de ejecución y privilegios.
11. Próximo: reportes humanos y JSON.
12. Próximo: persistencia/comparación entre ejecuciones.
13. Próximo: remoting y alcance seguro.
14. Próximo: concurrencia sólo donde aporte valor.
15. Próximo: debugging y profiling.
16. Próximo: hardening y entrega.
17. Próximo: evaluación final autónoma.

[Checkpoint 01 — Añade una regla diagnóstica](exercises/checkpoint-01.md) · [Solución de referencia](solutions/checkpoint-01.md)

## Qué sabrás hacer al terminar

Leer y escribir scripts PowerShell idiomáticos, trabajar con objetos y pipelines, diseñar funciones avanzadas, validar parámetros, manejar errores, importar módulos, consultar el sistema, producir reportes estructurados, escribir pruebas con Pester, depurar scripts y automatizar tareas operativas sin esconder efectos secundarios.

## Empleabilidad

PowerShell aparece con frecuencia en administración Windows, soporte/operaciones, Microsoft 365, Azure y automatización interna. Un puesto junior suele exigir además fundamentos de sistema operativo, permisos, redes, directorio/identidad o nube. Este curso prepara capacidades demostrables; no promete una vacante.

## Preguntas frecuentes

**¿Esto es lo mismo que CMD?** No. PowerShell transporta objetos por el pipeline y tiene un lenguaje completo, módulos, funciones, errores estructurados y acceso a .NET.

**¿Necesito Windows para empezar?** No para las primeras lecciones. El curso marca explícitamente las capacidades que sí dependen de Windows.

**¿Windows PowerShell 5.1 o PowerShell 7?** Aprendemos PowerShell 7.6 LTS. 5.1 sigue existiendo en Windows y se tratará como compatibilidad cuando corresponda.

**¿El auditor cambia configuración del equipo?** No. La aplicación canónica empieza como herramienta de lectura/diagnóstico. Las acciones que cambian estado no se mezclan silenciosamente con una auditoría.

## Glosario inicial

- **Cmdlet:** comando PowerShell que recibe y devuelve objetos.
- **Pipeline:** composición de comandos mediante `|`, pasando objetos, no sólo texto.
- **Objeto:** valor con propiedades y métodos.
- **Módulo:** unidad reutilizable de funciones/comandos PowerShell.
- **Pester:** framework de pruebas habitual del ecosistema PowerShell.
- **Finding / hallazgo:** conclusión diagnóstica respaldada por evidencia observable.

## Cómo hablar de este proyecto en una entrevista

Explica qué señales recopila WorkstationAudit, cómo separas recolección de reglas diagnósticas, qué haces cuando una consulta falla, por qué el reporte es estructurado y cómo pruebas reglas sin depender del hardware exacto del runner.

## Referencias oficiales

- [Documentación de PowerShell](https://learn.microsoft.com/powershell/)
- [Ciclo de soporte de PowerShell](https://learn.microsoft.com/powershell/scripting/install/powershell-support-lifecycle)
- [Pester](https://pester.dev/)

## Siguiente paso

Empieza con la lección 1 y llega al checkpoint sin copiar la solución antes de intentarlo. Para control de versiones y colaboración, usa el [curso transversal de Git](../git/).
