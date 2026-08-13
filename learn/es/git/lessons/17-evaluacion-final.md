# Lección 17 — Evaluación final Git Junior sin receta

## Qué vas a conseguir

Vas a demostrar que puedes leer, modificar, sincronizar, diagnosticar, recuperar y explicar un repositorio Git sin seguir una receta paso a paso. Esta lección no introduce un comando principal nuevo: integra el curso completo sobre ReleaseDesk.

## Antes de empezar

Trabaja sobre una copia independiente de `app/` y confirma primero que puedes explicar el estado actual:

```text
git status
git log --oneline --decorate --graph --all
git branch -vv
git remote -v
git tag --list
```

Si todavía no puedes distinguir `HEAD`, una branch local y `origin/main`, repasa antes de iniciar la evaluación.

## El problema

ReleaseDesk entra en una entrega importante. Hay trabajo local, un colaborador publicará cambios en paralelo, aparecerá una regresión que debes localizar y tendrás que recuperar trabajo cuya referencia fue eliminada. Además debes demostrar que sabes responder ante una exposición de credenciales sin convertir la limpieza de historia en una falsa sensación de seguridad.

Nadie te indicará una secuencia exacta de comandos.

## Concepto

La competencia inicial con Git no consiste en recordar muchas órdenes. Consiste en poder **observar → formular una hipótesis → elegir una operación compatible con la historia → verificar → explicar**.

En la evaluación se valora tanto llegar al estado correcto como conservar evidencia de por qué cada operación era apropiada.

## Demostración

[DEMO] Antes de modificar archivos, dibuja o explica verbalmente el grafo que ves con `git log --graph --all`. Señala qué referencias pueden moverse, cuáles representan observaciones remotas y qué objeto identifica un tag anotado.

No ejecutes una operación destructiva mientras no puedas explicar qué referencia u objeto afectará.

## Código real

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md) y resuelve el escenario completo sobre ReleaseDesk.

Puedes consultar:

- documentación oficial de Git;
- las lecciones anteriores;
- `git help` / `git <comando> --help`;
- `status`, `log`, `diff`, `show`, `reflog` y otras herramientas de evidencia.

No abras la solución de referencia hasta completar un intento verificable.

## Qué acaba de pasar

A partir de este punto ya no estás “haciendo ejercicios de comandos”. Estás operando un repositorio distribuido con historia, remotos, colaboración, errores recuperables y decisiones de gobierno.

## Errores comunes

- Ejecutar `reset --hard` porque el working tree “estorba” sin identificar qué perderías.
- Usar `push --force` para vencer un rechazo sin investigar la historia remota.
- Resolver un conflicto conservando sólo “mi versión” o “su versión” sin preservar la intención necesaria.
- Elegir manualmente el supuesto commit culpable y luego fingir que `bisect` lo encontró.
- Copiar el SHA de una branch antes de borrarla en un ejercicio de reflog.
- Creer que borrar un secreto de `HEAD` lo vuelve seguro.
- Tratar una firma como garantía de revisión, calidad o autorización.

## Buenas prácticas

Antes de una operación importante, captura evidencia mínima:

```text
git status --short
git log --oneline --decorate --graph --all -n 20
git diff
git diff --staged
```

Después de la operación, vuelve a comprobar el grafo y el estado. Si cambió algo que no esperabas, diagnostica antes de seguir apilando comandos.

## Tu turno

[PAUSA PARA EJERCICIO] Completa las historias A–F de la evaluación final. Entrega el repositorio, evidencia de comandos relevantes y una explicación breve de tus decisiones.

La evaluación incluye lectura de historia, commits coherentes, colaboración remota, diagnóstico con `bisect`, recuperación con reflog, tags, hooks/políticas y respuesta segura ante un secreto de laboratorio.

## Cómo comprobar

Como mínimo debes poder mostrar:

```text
git status --short
git log --oneline --decorate --graph --all
git branch -vv
git remote -v
git tag --list
git reflog --all
git fsck --no-reflogs --unreachable
```

`git fsck` se usa aquí sólo como evidencia adicional; no sustituye `reflog` ni constituye por sí mismo una estrategia de recuperación.

Evalúate con la [`rúbrica final`](../exercises/rubrica-final.md).

## Solución enlazada

Sólo después de completar un intento, compara tu trabajo con [`../solutions/evaluacion-final.md`](../solutions/evaluacion-final.md). La referencia describe una dirección razonable; no exige una historia idéntica commit por commit.

## Reto adicional

Explica cómo cambiaría tu estrategia si el repositorio tuviera 40 personas trabajando, branch protection, CI obligatorio y releases firmadas. Distingue claramente qué resuelve Git localmente y qué necesita una plataforma/gobierno compartido.

## Cómo hablar de este proyecto en una entrevista

Presenta ReleaseDesk como un laboratorio de control de versiones distribuido. Explica un incidente concreto: qué observaste, qué hipótesis formulaste, qué comando usaste y qué evidencia verificó el resultado.

Una explicación sólida puede cubrir:

- por qué staging permite construir commits intencionales;
- diferencia entre `fetch`, integración y `push`;
- cuándo elegirías merge o rebase;
- cómo resuelves un non-fast-forward sin borrar trabajo ajeno;
- cómo `bisect` reduce una búsqueda de regresión;
- qué puede recuperar reflog y por qué no es un backup;
- por qué los hooks locales no sustituyen CI/políticas de servidor;
- qué haces primero si una credencial real entra a la historia.

Preguntas probables:

- ¿Qué diferencia hay entre una branch y un tag?
- ¿Qué representa `origin/main`?
- ¿Cómo recuperarías un commit “perdido”?
- ¿Qué harías ante un conflicto que no entiendes?
- ¿Por qué evitarías `push --force` sobre historia compartida?
- ¿Qué significa que un commit esté firmado?

## Resumen

Completar Git significa poder trabajar con una historia existente, colaborar sin destruir cambios, diagnosticar regresiones, recuperar referencias y explicar límites de seguridad. La evaluación produce evidencia de competencia inicial; no garantiza contratación.

## Siguiente paso

Repite las áreas débiles de la rúbrica, conserva ReleaseDesk como evidencia y practica después sobre un repositorio ajeno real: primero leyendo historia y convenciones antes de modificarlo.

## Referencias

- [Git Reference](https://git-scm.com/docs)
- [Pro Git](https://git-scm.com/book/en/v2)
- [`git-log`](https://git-scm.com/docs/git-log)
- [`git-bisect`](https://git-scm.com/docs/git-bisect)
- [`git-reflog`](https://git-scm.com/docs/git-reflog)
- [`git-push`](https://git-scm.com/docs/git-push)
- [`githooks`](https://git-scm.com/docs/githooks)
