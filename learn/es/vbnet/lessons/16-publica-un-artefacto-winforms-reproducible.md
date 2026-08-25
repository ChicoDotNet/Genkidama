# Lección 16 — Publica un artefacto WinForms reproducible

## Qué vas a conseguir

Vas a demostrar que QuoteDesk no sólo compila: también puede producir un directorio publicable para Windows x64.

## El problema

Un build exitoso demuestra compilación, pero entrega y ejecución necesitan un artefacto claro. El pipeline debe poder repetir el mismo comando que usarías antes de empaquetar o instalar.

## Concepto

CI ejecuta `dotnet publish` para `win-x64` en modo framework-dependent. Esto demuestra empaquetado; **no** afirma que exista un instalador firmado ni una certificación de Microsoft Store.

## Cómo comprobarlo

```powershell
cd app
dotnet publish .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release -r win-x64 --self-contained false -o .\publish\quotedesk
```

Comprueba que `publish/quotedesk/QuoteDesk.WinForms.exe` existe.

## Buenas prácticas

- Distingue build, publish e installer.
- No guardes secretos en archivos de configuración publicados.
- Conserva backup y restore como parte de la preparación operativa.
- Documenta la plataforma que realmente probaste.

## Siguiente paso

Completa el [Checkpoint 04](../exercises/checkpoint-04.md). Después continúa con [Lección 17 — Evaluación final sin receta](17-evaluacion-final.md).

## Referencias
- [dotnet publish](https://learn.microsoft.com/dotnet/core/tools/dotnet-publish)
- [Deploy Windows Forms applications](https://learn.microsoft.com/dotnet/desktop/winforms/overview/)
