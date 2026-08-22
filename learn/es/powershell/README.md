# Curso de PowerShell desde cero — Construye un auditor de estaciones de trabajo

PowerShell es un shell y lenguaje de automatización orientado a objetos, muy usado para administrar Windows y servicios Microsoft y también disponible en Linux y macOS. **Puedes empezar desde cero aquí**: aprenderás el lenguaje construyendo **WorkstationAudit**, un auditor local que convierte señales del equipo en hallazgos y reportes explicables.

## Qué vas a construir

WorkstationAudit inspecciona plataforma, almacenamiento, señales Windows, inventario acotado y contexto de ejecución; transforma datos en hallazgos, exporta JSON/texto, compara ejecuciones, resume múltiples reportes con concurrencia acotada y mantiene límites explícitos para remoting sin modificar el equipo auditado.

El curso enseña PowerShell 7 moderno. Al 22 de agosto de 2026 Microsoft identifica **PowerShell 7.6 como la línea LTS vigente**. Windows PowerShell 5.1 aparece sólo cuando sea necesario para explicar compatibilidad.

## Requisitos
- PowerShell 7.6 LTS recomendado.
- Windows 11 para la experiencia objetivo del auditor.
- Linux actual sirve para practicar capacidades portables y pruebas del núcleo.
- VS Code es opcional.

## Ejecutar la aplicación

```powershell
./app/Invoke-Audit.ps1 -OutputPath ./audit.json -TextOutputPath ./audit.txt
./app/Invoke-Audit.ps1 -CompareWith ./audit.json
```

## Ejecutar pruebas

```powershell
Install-Module Pester -MinimumVersion 5.7.1 -Scope CurrentUser
Invoke-Pester ./app/tests
```

## Avance

**17/17 lecciones implementadas.** Cuatro checkpoints y una evaluación final autónoma cubren diagnóstico reproducible, fronteras Windows, política configurable, inventario, privilegios, reportes, comparación semántica, alcance remoto explícito, concurrencia acotada, medición y hardening.

1. [Lección 01 — Ejecuta tu primer auditor](lessons/01-ejecuta-tu-primer-auditor.md)
2. [Lección 02 — Trabaja con objetos y pipeline](lessons/02-objetos-y-pipeline.md)
3. [Lección 03 — Convierte señales en hallazgos](lessons/03-funciones-parametros-y-hallazgos.md)
4. [Lección 04 — Maneja errores y prueba comportamiento](lessons/04-errores-y-pruebas.md)
5. [Lección 05 — Configura reglas sin editar código](lessons/05-configura-reglas-sin-editar-codigo.md)
6. [Lección 06 — Consulta Windows con CIM](lessons/06-consulta-windows-con-cim.md)
7. [Lección 07 — Reglas reutilizables de memoria](lessons/07-reglas-reutilizables-de-memoria.md)
8. [Lección 08 — Compón un diagnóstico reproducible](lessons/08-compone-un-diagnostico-reproducible.md)
9. [Lección 09 — Inventario acotado](lessons/09-inventario-acotado.md)
10. [Lección 10 — Privilegios y seguridad](lessons/10-privilegios-y-seguridad.md)
11. [Lección 11 — Reportes humanos y JSON](lessons/11-reportes-humanos-y-json.md)
12. [Lección 12 — Compara auditorías](lessons/12-compara-auditorias.md)
13. [Lección 13 — Remoting con alcance explícito](lessons/13-remoting-con-alcance-explicito.md)
14. [Lección 14 — Fan-out con concurrencia acotada](lessons/14-concurrencia-acotada.md)
15. [Lección 15 — Mide antes de optimizar](lessons/15-mide-antes-de-optimizar.md)
16. [Lección 16 — Hardening y contrato de entrega](lessons/16-hardening-y-entrega.md)
17. [Lección 17 — Evaluación final autónoma](lessons/17-evaluacion-final.md)

[Checkpoint 01](exercises/checkpoint-01.md) · [Solución 01](solutions/checkpoint-01.md) · [Checkpoint 02](exercises/checkpoint-02.md) · [Solución 02](solutions/checkpoint-02.md) · [Checkpoint 03](exercises/checkpoint-03.md) · [Solución 03](solutions/checkpoint-03.md) · [Checkpoint 04](exercises/checkpoint-04.md) · [Solución 04](solutions/checkpoint-04.md) · [Evaluación final](exercises/final-workstationaudit.md) · [Referencia final](solutions/final-workstationaudit.md)

## Qué sabrás hacer al terminar
Leer y escribir scripts PowerShell idiomáticos, trabajar con objetos/pipelines, diseñar funciones, validar parámetros, manejar errores, importar módulos, consultar el sistema, producir reportes estructurados, escribir pruebas con Pester, depurar scripts y automatizar tareas operativas sin esconder efectos secundarios.

## Empleabilidad
PowerShell aparece con frecuencia en administración Windows, soporte/operaciones, Microsoft 365, Azure y automatización interna. Un puesto junior suele exigir además fundamentos de sistema operativo, permisos, redes, identidad o nube. Este curso prepara capacidades demostrables; no promete una vacante.

## Preguntas frecuentes
**¿Esto es lo mismo que CMD?** No. PowerShell transporta objetos por el pipeline y tiene un lenguaje completo.

**¿Necesito Windows para empezar?** No. El curso marca explícitamente las capacidades que sí dependen de Windows.

**¿Windows PowerShell 5.1 o PowerShell 7?** Aprendemos PowerShell 7.6 LTS.

**¿El auditor cambia configuración del equipo?** No. La aplicación canónica es de lectura/diagnóstico; las acciones que cambian estado no se mezclan silenciosamente.

## Glosario inicial
- **Cmdlet:** comando PowerShell que recibe y devuelve objetos.
- **Pipeline:** composición mediante `|`, pasando objetos.
- **Módulo:** unidad reutilizable de funciones PowerShell.
- **Finding:** conclusión diagnóstica respaldada por evidencia.
- **CIM:** modelo/interfaz de administración usada para consultar información de sistema.
- **Execution Policy:** configuración de condiciones de ejecución; no sustituye autorización ni otras fronteras de seguridad.
- **Baseline:** auditoría persistida usada como referencia para detectar cambios posteriores.
- **Throttle:** límite de trabajo concurrente activo.

## Cómo hablar de este proyecto en una entrevista
Explica cómo separas recolección, política, findings y presentación; por qué el inventario tiene límites; cómo haces visible la elevación; por qué JSON y texto nacen del mismo objeto; cómo comparas findings por identidad; y por qué el remoting requiere opt-in en lugar de modificar WinRM automáticamente. La lección 17 incluye una guía completa para explicar problema, arquitectura, trade-offs, pruebas y mejora futura sin inflar tu experiencia.

## Referencias oficiales
- https://learn.microsoft.com/powershell/
- https://learn.microsoft.com/powershell/scripting/install/powershell-support-lifecycle
- https://learn.microsoft.com/powershell/module/cimcmdlets/get-ciminstance
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/foreach-object
- https://pester.dev/

## Siguiente paso
Completa la evaluación final sin copiar la referencia. Para control de versiones y colaboración, usa el [curso transversal de Git](../git/). Después puedes continuar con el siguiente curso del roadmap de Genkidama Learn.
