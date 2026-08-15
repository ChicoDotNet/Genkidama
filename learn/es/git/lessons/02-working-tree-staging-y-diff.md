# Lección 02 — Working tree, staging y diff

## Qué vas a conseguir

Harás dos cambios distintos en ReleaseDesk, inspeccionarás sus diferencias y prepararás sólo uno para demostrar que **staging no significa “guardar todo”**.

## Antes de empezar

Debes haber completado la [Lección 01](01-primer-repositorio-y-primer-commit.md) y tener `main` limpio:

```text
git status --short
```

Si aparece salida, entiende primero qué cambio quedó pendiente.

## El problema

Imagina que durante una misma sesión:

- mejoras el plan de la entrega;
- corriges el changelog;
- además dejas una nota temporal.

Si conviertes todo en un único commit sólo porque ocurrió al mismo tiempo, la historia empieza a representar sesiones de trabajo en vez de decisiones coherentes.

## Concepto

Git te deja observar tres comparaciones importantes:

```text
working tree  --git diff-->          staging/index
staging       --git diff --staged--> último commit (HEAD)
HEAD          ---------------------> historia registrada
```

`git add <archivo>` actualiza el contenido preparado para el próximo commit. Puedes seguir editando ese archivo después: la versión preparada y la versión visible pueden incluso ser distintas.

## Demostración

[DEMO] Edita `docs/plan.md` y agrega bajo `## Alcance`:

```markdown
- registrar cada cambio como una unidad revisable;
```

Después agrega al final de `CHANGELOG.md`:

```markdown

## Próxima entrega

- Preparar una plantilla de incidentes.
```

Ejecuta:

```text
git status
git diff
```

Ahora prepara **sólo** el plan:

```text
git add docs/plan.md
git status
git diff
git diff --staged
```

## Código real

El objetivo es producir dos commits con intenciones separadas:

```text
git commit -m "docs: aclarar alcance de ReleaseDesk"
git status
git diff
git add CHANGELOG.md
git diff --staged
git commit -m "docs: preparar próxima entrega"
git status
```

Observa que el primer commit no absorbió el cambio del changelog.

## Qué acaba de pasar

`git diff` cambió de significado observable conforme moviste contenido entre working tree y staging. No hubo magia ni “guardado especial”: Git comparó estados distintos.

El staging area permite construir el próximo commit intencionalmente incluso cuando tu directorio contiene trabajo de más de una idea.

## Errores comunes

- Ejecutar siempre `git add .` antes de mirar `git status`.
- Confundir `git diff` con `git diff --staged`.
- Suponer que un archivo está “completo” en staging porque lo agregaste una vez; si lo vuelves a editar, aparecen cambios nuevos fuera del staging.
- Hacer commits gigantes que mezclan documentación, bugfixes y experimentos sin relación.
- Usar una GUI sin entender qué parte corresponde a working tree y cuál a staged changes.

## Buenas prácticas

Un ciclo pequeño y confiable es:

```text
git status
git diff
git add <archivos conscientes>
git diff --staged
git commit -m "mensaje con intención"
```

No es la única forma de trabajar, pero es una excelente rutina mientras construyes criterio.

## Tu turno

1. Agrega una línea útil a `README.md`.
2. Crea `tmp/nota.txt` con cualquier texto.
3. Ejecuta `git status --short`.
4. Explica por qué `tmp/nota.txt` no aparece.
5. Prepara `README.md`.
6. Revisa `git diff --staged`.
7. Antes de hacer commit, agrega una segunda línea a `README.md`.
8. Compara `git diff` y `git diff --staged` para comprobar que un mismo archivo puede tener contenido preparado y no preparado.

[PAUSA PARA EJERCICIO]

## Cómo comprobar

Debes poder responder con evidencia:

- ¿qué versión de `README.md` entraría al commit si lo hicieras ahora?;
- ¿qué cambio permanece sólo en working tree?;
- ¿por qué `tmp/nota.txt` está ignorado?;
- ¿qué comando mostraría únicamente lo preparado?

Después decide si quieres preparar la segunda línea y crea un commit coherente.

## Solución

La respuesta central es observable:

```text
git diff
git diff --staged
git status --short
```

No borres `tmp/` del `.gitignore`; el objetivo es comprobar que una regla de ignore puede mantener artefactos temporales fuera del flujo normal.

## Reto adicional

Ejecuta:

```text
git ls-files
```

Compara la lista de archivos rastreados con todo lo que existe físicamente en la carpeta.

## Resumen

- working tree y staging son estados diferentes;
- `git diff` y `git diff --staged` responden preguntas diferentes;
- `git add` prepara contenido, no “activa seguimiento mágico” para siempre;
- commits pequeños y coherentes mejoran revisión y recuperación;
- `.gitignore` evita ruido, no borra archivos del disco.

## Siguiente paso

Continúa con [Lección 03 — Historia y recuperación segura](03-historia-y-recuperacion-segura.md). Vas a inspeccionar commits y deshacer errores locales sin empezar por operaciones destructivas.

## Referencias

- [`git-diff`](https://git-scm.com/docs/git-diff)
- [`git-add`](https://git-scm.com/docs/git-add)
- [`gitignore`](https://git-scm.com/docs/gitignore)
- [Recording Changes to the Repository — Pro Git](https://git-scm.com/book/en/v2/Git-Basics-Recording-Changes-to-the-Repository)
