# Lección 13 — Remoting con alcance explícito

## Qué vas a conseguir
Distinguirás una auditoría local de un destino remoto sin activar ni modificar remoting de forma silenciosa.

## El problema
`Invoke-Command` hace sencillo ejecutar código en otra máquina, pero que algo sea técnicamente posible no significa que debamos cambiar WinRM, firewall o autenticación desde un auditor.

## Concepto
`Resolve-AuditTarget` normaliza el destino y exige `-AllowRemote` para cualquier equipo que no sea local.

```powershell
Resolve-AuditTarget -ComputerName localhost
Resolve-AuditTarget -ComputerName server-01 -AllowRemote
```

El resultado indica `Mode`, `RequiresRemoting` y confirma `ChangesSystemConfiguration = false`.

WorkstationAudit no habilita WinRM ni modifica TrustedHosts. La preparación del entorno remoto pertenece a operaciones y seguridad, no al diagnóstico.

## Tu turno
Prueba un destino remoto sin `-AllowRemote` y verifica que falle. Después repítelo con el switch y explica qué cambia y qué no.

## Cómo comprobar tu solución
`Invoke-Pester ./app/tests` cubre ambos caminos sin necesitar una segunda máquina.

## Siguiente paso
Continúa con [Lección 14 — Fan-out con concurrencia acotada](14-concurrencia-acotada.md).

## Referencias
- https://learn.microsoft.com/powershell/scripting/security/remoting/powershell-remoting-faq
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/invoke-command
