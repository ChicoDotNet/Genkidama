# Checkpoint 02 — Sincroniza, resuelve y actualiza una feature

## Objetivo

Demostrar que entiendes Git distribuido sin depender de GitHub ni de Internet.

Trabajarás con:

- un repositorio bare que representa `origin`;
- dos clones, `alpha` y `beta`;
- una feature privada que todavía no usa otra persona.

## Parte A — observar antes de integrar

1. Desde `beta`, agrega una mejora pequeña a `docs/plan.md`, haz commit y publícala a `origin/main`.
2. En `alpha`, **no ejecutes pull de inmediato**. Ejecuta `git fetch origin`.
3. Demuestra con `git log --graph` que `origin/main` avanzó mientras `main` local todavía apunta al commit anterior.
4. Si no existe divergencia local, sincroniza `main` mediante fast-forward explícito.

## Parte B — conflicto deliberado

1. Haz que ambos clones partan del mismo estado.
2. En `alpha`, modifica una línea existente de `CHANGELOG.md` y haz commit local sin publicar.
3. En `beta`, modifica esa misma línea de otra forma, haz commit y publícala a `origin/main`.
4. En `alpha`, ejecuta `fetch` y trata de integrar `origin/main`.
5. Lee `status` y `diff` antes de resolver.
6. Produce una resolución final que conserve la intención válida de ambos cambios, prepara el archivo y completa el merge.
7. Publica la reconciliación.

## Parte C — feature privada y rebase

1. Desde `alpha/main` sincronizada crea `feature/checkpoint-02`.
2. Agrega `docs/release-owner.md` y registra el commit.
3. Sin publicar todavía esa feature, desde `beta/main` agrega una mejora distinta a `README.md` y publícala.
4. En `alpha`, estando en `feature/checkpoint-02`, ejecuta `fetch` y rebasa la feature sobre `origin/main`.
5. Comprueba que la feature contiene sólo su intención sobre la base nueva.
6. Publica la branch de feature por primera vez.

## Evidencia que debes entregar

Incluye la salida relevante de:

```text
git status --short
git branch -vv
git log --oneline --decorate --graph --all
git log --oneline origin/main..feature/checkpoint-02
git diff --check origin/main...feature/checkpoint-02
git diff --stat origin/main...feature/checkpoint-02
```

Y responde brevemente:

1. ¿Por qué `fetch` no movió tu `main`?
2. ¿Qué información necesitaste para resolver el conflicto correctamente?
3. ¿Por qué elegiste merge para reconciliar la divergencia de `main`?
4. ¿Por qué fue razonable rebasar `feature/checkpoint-02`?
5. ¿Qué cambiaría si otra persona ya hubiera basado trabajo sobre esa feature?

## Criterios de éxito

- no se pierde ninguna intención válida;
- terminas con working trees limpios;
- `origin/main` contiene la resolución conflictiva;
- la feature queda basada en la `main` remota más reciente;
- puedes explicar la diferencia entre observar, integrar, publicar y reescribir historia.

Cuando termines un intento completo, compara con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).
