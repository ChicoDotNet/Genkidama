# Lección 11 — Reportes deterministas

## Qué vas a conseguir

Vas a convertir una `Quote` en texto estable antes de tocar el filesystem. Separar render de persistencia permite probar el contenido sin crear archivos.

`Reporting.render` usa `CultureInfo.InvariantCulture` para que `250.00` no cambie a `250,00` según la máquina. Ese detalle importa cuando un archivo es contrato entre procesos, evidencia o entrada futura.

## Diseño

La función principal es pura:

```fsharp
Quote -> string
```

Sólo `Reporting.save` realiza I/O. El reporte incluye subtotal, tasa, descuento, total y partidas en orden.

## Ejercicio

Agrega una línea `LineCount=<n>` al reporte. Primero escribe una prueba sobre `Reporting.render`; después implementa el cambio sin tocar `Reporting.save`.

## Qué probar

Protege formato y orden que realmente sean contrato. Evita tests que sólo repitan cada línea de implementación.

## Referencias oficiales

- [CultureInfo.InvariantCulture](https://learn.microsoft.com/dotnet/api/system.globalization.cultureinfo.invariantculture)
- [F# string formatting](https://learn.microsoft.com/dotnet/fsharp/language-reference/strings)

[Anterior](10-tipos-para-fronteras.md) · [Siguiente](12-persistencia-y-fallos.md)
