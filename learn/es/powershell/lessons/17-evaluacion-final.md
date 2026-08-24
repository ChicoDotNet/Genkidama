# Lección 17 — Evaluación final autónoma

## Qué vas a demostrar
Que puedes leer, modificar, probar y explicar WorkstationAudit sin seguir una receta paso a paso.

Esta lección no introduce sintaxis nueva. La evidencia está en resolver el [reto final](../exercises/final-workstationaudit.md), mantener los tests existentes verdes y añadir pruebas para tu cambio.

## Antes de empezar

Ejecuta primero la baseline:

```powershell
Invoke-Pester ./app/tests
./app/Invoke-Audit.ps1 -OutputPath ./audit.json -TextOutputPath ./audit.txt
```

Después trabaja sin consultar la solución de referencia.

## Qué se evalúa

- comprensión de la arquitectura actual;
- cambio funcional pequeño pero real;
- corrección de un bug o failure mode;
- manejo explícito de errores;
- prueba Pester nueva;
- uso de documentación oficial;
- diseño de una mejora futura con trade-offs;
- capacidad para explicar seguridad, concurrencia y límites de plataforma.

## Cómo hablar de este proyecto en una entrevista

Explica el problema antes que los cmdlets. Una respuesta sólida cubre:

1. **Problema:** obtener evidencia diagnóstica reproducible sin modificar el equipo.
2. **Arquitectura:** observación → configuración/política → findings → persistencia/presentación.
3. **Errores reales:** por ejemplo, entradas del Registro sin `DisplayName` bajo StrictMode.
4. **Pruebas:** fixtures deterministas más smoke real en Windows y Ubuntu.
5. **Seguridad:** el auditor no eleva privilegios, no habilita WinRM y exige opt-in para destinos remotos.
6. **Concurrencia:** se usa sólo en fan-out de reportes independientes, con throttle y equivalencia semántica contra ejecución secuencial.
7. **Mejora futura:** propone una, explica valor, riesgo y cómo la probarías.

No presentes este curso como experiencia profesional que no tienes. Presenta lo que construiste, las decisiones que puedes defender y lo que todavía necesitarías aprender.

## Cierre

Cuando termines la evaluación, compara tu trabajo con la rúbrica. Si puedes explicar por qué tu solución es segura, comprobable y mantenible —no sólo que «funciona en tu PC»— ya alcanzaste el objetivo 0 → Junior de este curso.

## Referencias
- https://learn.microsoft.com/powershell/
- https://learn.microsoft.com/powershell/scripting/learn/deep-dives/everything-about-exceptions
- https://pester.dev/docs/quick-start
