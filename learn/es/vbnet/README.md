# Curso de Visual Basic .NET desde cero — Construye un gestor WinForms de cotizaciones y facturas

Visual Basic .NET comparte runtime, bibliotecas y tooling con el resto de .NET. En este curso construyes **QuoteDesk**, una aplicación Windows Forms con un núcleo portable para preparar cotizaciones, aprobarlas, generar facturas, persistir documentos y diagnosticar fallas sin esconderlas.

El curso usa **.NET 10 LTS**, `Option Strict On`, `Option Explicit On` y `Option Infer On`. La interfaz es Windows; el núcleo y sus pruebas también se ejecutan en Ubuntu para demostrar que las reglas no dependen de WinForms.

## Aplicación canónica

QuoteDesk cubre:

- partidas, cantidades y precios con `Decimal`;
- borradores editables y cotizaciones aprobadas congeladas;
- impuestos y redondeo determinista;
- conversión a facturas inmutables;
- JSON versionado con validación de datos externos;
- catálogo y búsqueda de documentos;
- errores de persistencia observables;
- diagnóstico read-only sin nombres ni descripciones de clientes;
- backup no destructivo;
- build y publish WinForms reproducibles.

## Requisitos

- .NET 10 SDK.
- Windows para ejecutar la UI WinForms.
- Ubuntu o Windows sirven para trabajar con el núcleo y sus tests.

## Build, test y publish

```powershell
cd .\app
dotnet test .\QuoteDesk.Tests\QuoteDesk.Tests.vbproj -c Release
dotnet build .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release
dotnet publish .\QuoteDesk.WinForms\QuoteDesk.WinForms.vbproj -c Release -r win-x64 --self-contained false -o .\publish\quotedesk
```

## Lecciones

1. [Ejecuta VB.NET y conoce QuoteDesk](lessons/01-ejecuta-vbnet-y-conoce-quotedesk.md)
2. [Modela una partida con tipos explícitos](lessons/02-modela-una-partida-con-tipos-explicitos.md)
3. [Calcula una cotización con objetos](lessons/03-calcula-una-cotizacion-con-objetos.md)
4. [Separa la vista con un presenter](lessons/04-separa-la-vista-con-un-presenter.md)
5. [Edita una cotización sin romper sus reglas](lessons/05-edita-una-cotizacion-sin-romper-sus-reglas.md)
6. [Calcula impuestos y aprueba la cotización](lessons/06-calcula-impuestos-y-aprueba-la-cotizacion.md)
7. [Convierte una cotización aprobada en factura](lessons/07-convierte-una-cotizacion-aprobada-en-factura.md)
8. [Guarda y recupera cotizaciones con JSON](lessons/08-guarda-y-recupera-cotizaciones-con-json.md)
9. [Lista y busca cotizaciones guardadas](lessons/09-lista-y-busca-cotizaciones-guardadas.md)
10. [Haz visibles los errores de persistencia](lessons/10-haz-visibles-los-errores-de-persistencia.md)
11. [Trata JSON como datos externos no confiables](lessons/11-trata-json-como-datos-externos-no-confiables.md)
12. [Persiste la factura como snapshot](lessons/12-persiste-la-factura-como-snapshot.md)
13. [Diagnostica sin exponer datos del cliente](lessons/13-diagnostica-sin-exponer-datos-del-cliente.md)
14. [Depura con evidencia reproducible](lessons/14-depura-con-evidencia-reproducible.md)
15. [Respalda antes de recuperar](lessons/15-respalda-antes-de-recuperar.md)
16. [Publica un artefacto WinForms reproducible](lessons/16-publica-un-artefacto-winforms-reproducible.md)
17. [Evaluación final sin receta](lessons/17-evaluacion-final.md)

## Checkpoints y evaluación

- [Checkpoint 01](exercises/checkpoint-01.md) · [Solución](solutions/checkpoint-01.md)
- [Checkpoint 02](exercises/checkpoint-02.md) · [Solución](solutions/checkpoint-02.md)
- [Checkpoint 03](exercises/checkpoint-03.md) · [Solución](solutions/checkpoint-03-solution.md)
- [Checkpoint 04](exercises/checkpoint-04.md) · [Solución](solutions/checkpoint-04-solution.md)
- [Evaluación final](exercises/evaluacion-final.md) · [Rúbrica](exercises/rubrica-final.md) · [Solución de referencia](solutions/evaluacion-final.md)

## Cómo hablar de QuoteDesk en una entrevista

Describe la separación entre dominio portable y UI, el ciclo Draft→Approved, por qué la factura es un snapshot, cómo se validan archivos externos y cómo CI prueba el núcleo en dos sistemas operativos mientras compila/publica WinForms sólo en Windows. Habla también de un error real que encontraste y de la regresión que evita que vuelva.

## FAQ

### ¿VB.NET es lo mismo que VB6 o VBA?

No. VB.NET compila para .NET y usa su runtime y bibliotecas. VB6 y VBA pertenecen a ecosistemas distintos.

### ¿Por qué probar en Linux una app WinForms?

No se prueba WinForms allí. Se prueba el núcleo portable para comprobar que las reglas de negocio no dependen accidentalmente de Windows.

### ¿Por qué usar `Decimal` para dinero?

Porque modela aritmética decimal sin los errores binarios típicos de `Double`; aun así debes definir reglas de redondeo.

### ¿Un archivo local es confiable?

No por definición. Puede estar editado, truncado o venir de otra versión. QuoteDesk valida schema y contratos antes de reconstruir objetos.

### ¿`dotnet publish` crea un instalador?

No. Produce artefactos listos para deployment según la configuración; firma, instalador y distribución son pasos distintos.

## Glosario

- **LTS:** versión con soporte prolongado.
- **Presenter:** objeto que coordina una vista pasiva con lógica testeable sin depender de controles concretos.
- **Snapshot:** copia inmutable del estado relevante en un momento.
- **Schema version:** número que permite decidir si un documento persistido puede interpretarse.
- **PII:** información que puede identificar a una persona; el diagnóstico evita datos innecesarios del cliente.
- **Framework-dependent publish:** publicación que requiere el runtime .NET compatible instalado en destino.

## Git

Para ramas, historial, recuperación y colaboración usa el [curso transversal de Git](../git/); este curso no duplica esa materia.

## Referencias oficiales

- [.NET support policy](https://dotnet.microsoft.com/platform/support/policy)
- [Visual Basic](https://learn.microsoft.com/dotnet/visual-basic/)
- [Windows Forms](https://learn.microsoft.com/dotnet/desktop/winforms/)
- [System.Text.Json](https://learn.microsoft.com/dotnet/standard/serialization/system-text-json/overview)
- [.NET testing](https://learn.microsoft.com/dotnet/core/testing/)
- [`dotnet publish`](https://learn.microsoft.com/dotnet/core/tools/dotnet-publish)

**Contenido completo: 17/17 lecciones.** La promoción final depende de CI y de reconciliar la cadena de PR con `dev`.
