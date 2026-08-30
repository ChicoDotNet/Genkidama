# Lección 15 — Verifica una entrega portable

## Qué vas a conseguir
Convertirás “los tests pasan” en una señal de entrega más completa: formato, análisis estático, tests y un artefacto Flutter construible.

## El problema
Una suite verde no garantiza que el proyecto pueda empaquetarse. Dependencias, assets y configuración pueden fallar sólo en la fase de build.

## Concepto
El gate de PocketLedger separa cuatro señales:

```text
restore -> format -> analyze -> test -> build
```

Cada una responde una pregunta distinta. El formatter mantiene una representación canónica; el analyzer encuentra problemas estáticos; tests protegen comportamiento; build demuestra que Flutter puede producir un artefacto.

Para CI usamos una construcción web como prueba portable de packaging. Eso **no significa** que el curso afirme publicación Android/iOS certificada: esas plataformas tienen toolchains, firmas y tiendas adicionales.

## Código real
Ver gate consolidado: [`../../../../.github/workflows/polyglot.yml`](../../../../.github/workflows/polyglot.yml).

[EJECUTAR]

```bash
cd learn/es/dart/app
flutter pub get
dart format lib test
flutter analyze
flutter test
flutter build web --release
```

## Buenas prácticas
- No llames “soportada” a una plataforma que no ejecutaste.
- Mantén el build reproducible desde una copia limpia.
- Si un paquete anuncia deprecación, trátalo como señal de mantenimiento, no la ocultes.

## Tu turno
Explica por qué un build web verde no demuestra que una app iOS está lista para App Store. Después ejecuta el pipeline completo disponible en tu equipo.

## Resumen
La entrega es una cadena de evidencia. Ningún paso individual sustituye a los demás.

## Siguiente paso
[Lección 16 — Hardening antes de entregar](16-hardening-antes-de-entregar.md)

## Referencias
- [Flutter build and release](https://docs.flutter.dev/deployment)
- [Flutter web deployment](https://docs.flutter.dev/deployment/web)
