# Lección 07 — Pull Requests y revisión de cambios

## Qué vas a conseguir

Prepararás una branch para revisión como si fuera a abrirse un Pull Request. Aprenderás a inspeccionar la intención completa de una branch antes de integrarla y separarás claramente Git de la plataforma que hospeda la conversación de revisión.

## Antes de empezar

Completa la [Lección 06](06-conflictos-de-merge-sin-panico.md). Deja `main` sincronizada y limpia.

## El problema

Una branch puede compilar, estar limpia y aun así contener una mala decisión. Integrar no debería ser sólo una operación mecánica: alguien necesita revisar qué cambia, por qué cambia y si el conjunto es coherente.

## Concepto

Un Pull Request no es una primitiva de Git. Plataformas como GitHub construyen una conversación y un flujo de aprobación alrededor de dos referencias Git: una base y una branch propuesta.

Antes de abrir una PR puedes inspeccionar localmente casi toda la evidencia técnica básica:

```text
git log origin/main..feature/release-summary
git diff origin/main...feature/release-summary
```

`..` y `...` no significan lo mismo. Para revisión de una feature, el diff con tres puntos compara la branch contra su ancestro común con la base y representa mejor la intención agregada por la feature.

## Demostración

Crea una branch y agrega `docs/release-checklist.md` con una checklist breve de publicación.

```text
git switch -c feature/release-summary
git add docs/release-checklist.md
git commit -m "docs: agregar checklist de entrega"
git push -u origin feature/release-summary
```

Inspecciona antes de pedir revisión:

```text
git status --short
git log --oneline origin/main..HEAD
git diff --stat origin/main...HEAD
git diff origin/main...HEAD
```

## Código real

Una descripción de PR útil debería permitir responder:

- ¿qué problema resuelve este cambio?;
- ¿qué archivos y comportamiento modifica?;
- ¿cómo se comprobó?;
- ¿qué riesgo o limitación queda?;
- ¿qué no forma parte del alcance?

En GitHub, la PR agrega comentarios, reviewers, checks y políticas de integración. Pero la evidencia que revisas sigue siendo historia y diff de Git.

## Qué acaba de pasar

Publicar la branch no la integró. Abrir una PR tampoco la integra por sí mismo. La branch propuesta puede seguir recibiendo commits mientras la revisión ocurre; por eso conviene revisar el head actual y no una impresión antigua.

## Errores comunes

- Abrir PRs gigantes sin una unidad de intención clara.
- Revisar sólo archivos sueltos y perder la historia completa de la branch.
- Confundir “checks verdes” con “diseño correcto”.
- Cambiar el alcance durante la revisión sin explicarlo.
- Usar el título de la PR como sustituto de una descripción verificable.

## Buenas prácticas

Antes de publicar una branch para revisión:

```text
git fetch origin
git status --short
git log --oneline origin/main..HEAD
git diff --check origin/main...HEAD
git diff --stat origin/main...HEAD
```

Después revisa el diff completo. Un reviewer debería poder entender la intención sin reconstruirla a partir de veinte commits accidentales.

## Tu turno

Crea una branch `feature/release-owner`, agrega al plan una sección que identifique responsable de una entrega y registra uno o dos commits coherentes. Publica la branch en tu remoto local y prepara, en un archivo de notas, una descripción de PR con problema, cambio, comprobación y riesgo.

No necesitas GitHub para completar la parte técnica del ejercicio. Si trabajas en un repositorio remoto real con permisos, puedes abrir una PR después de inspeccionar exactamente la misma evidencia.

## Cómo comprobar

```text
git log --oneline origin/main..HEAD
git diff --check origin/main...HEAD
git diff --stat origin/main...HEAD
```

Debes poder explicar cada commit que aparece y cada archivo modificado.

## Reto adicional

Haz un segundo commit irrelevante en la branch y pregúntate si pertenece a la misma PR. Si no, no lo integres sólo porque ya está ahí: la capacidad profesional es reconocer el límite del cambio.

## Resumen

- Pull Request es una capa de colaboración sobre referencias Git;
- `origin/main...HEAD` permite revisar la intención de una feature desde su ancestro común;
- una PR útil explica problema, cambio, validación y riesgo;
- checks verdes son evidencia necesaria, no aprobación arquitectónica automática;
- publicar una branch no equivale a integrarla.

## Siguiente paso

Continúa con la [Lección 08 — Rebase consciente y checkpoint 02](08-rebase-consciente-y-checkpoint.md).

## Referencias

- [`git-diff`](https://git-scm.com/docs/git-diff)
- [`git-log`](https://git-scm.com/docs/git-log)
- [Distributed Git — Contributing to a Project](https://git-scm.com/book/en/v2/Distributed-Git-Contributing-to-a-Project)
- [GitHub Docs — About pull requests](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests)
