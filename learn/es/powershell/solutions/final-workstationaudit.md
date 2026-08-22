# Solución de referencia — Evaluación final WorkstationAudit

Esta referencia no define una única respuesta correcta. Un enfoque válido podría añadir una regla sobre antigüedad del último arranque usando `System.LastBootUpTime`, con umbral configurable y fixture determinista.

La implementación de referencia debería mantener estas fronteras:

- el proveedor recopila la fecha;
- la regla decide la severidad;
- el finding conserva evidencia estructurada;
- `Get-WorkstationAudit` compone sin consultar dos veces la misma señal;
- Pester cubre un caso normal y uno con fecha ausente/inválida;
- el reporte continúa siendo read-only.

Un failure mode razonable es que `LastBootUpTime` no exista o no sea convertible. En vez de asumir `DateTime.MinValue`, la solución puede devolver un finding `uptime.unknown` de severidad `Info` o lanzar una excepción contextual si el contrato exige la señal; lo importante es que la decisión sea explícita y esté probada.

Una mejora futura defendible sería recopilar varios equipos mediante un sistema de administración ya gobernado por la organización. No corresponde que WorkstationAudit habilite WinRM, cambie TrustedHosts o distribuya credenciales por su cuenta.

Documentación oficial útil para contrastar decisiones:
- https://learn.microsoft.com/powershell/module/cimcmdlets/get-ciminstance
- https://learn.microsoft.com/powershell/module/microsoft.powershell.core/about/about_strict_mode
- https://pester.dev/
