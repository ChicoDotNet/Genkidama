# Checkpoint 02 — De borrador editable a factura persistida

Trabaja sin copiar la solución.

## Objetivo

Demuestra que puedes usar las capacidades de las lecciones 05–08 como un flujo coherente.

## Tareas

1. Crea una cotización para un cliente con dos partidas.
2. Reemplaza una partida y elimina la otra.
3. Configura una tasa válida y comprueba subtotal, impuesto y total.
4. Aprueba la cotización.
5. Comprueba que una edición posterior falla explícitamente.
6. Guarda la cotización en JSON y vuelve a cargarla.
7. Crea una factura `F-CHK-02` desde la cotización recuperada.
8. Añade al menos una prueba que demuestre el flujo completo o una frontera de error relevante.

## Evidencia esperada

- pruebas verdes;
- el archivo persistido existe y vuelve a producir el mismo total;
- la factura conserva cliente, partidas e importes;
- un borrador no puede facturarse.

## Cómo comprobar

```powershell
cd app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
```

Cuando termines, compara tu enfoque con la [solución de referencia](../solutions/checkpoint-02.md).
