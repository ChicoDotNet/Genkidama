# Solución de referencia — Evaluación final Git

> Abre esta referencia sólo después de completar un intento. No existe una única historia correcta.

La evaluación mide criterio operativo, no similitud de comandos. Una solución válida puede elegir merge o rebase en distintos puntos siempre que preserve historia, explique la decisión y deje evidencia verificable.

## Historia A — Modelo mental

Una lectura fuerte empieza por separar:

- `HEAD`: el contexto actualmente seleccionado;
- branch local: referencia móvil que controlas en tu repositorio;
- `origin/main`: observación local de la última referencia remota obtenida;
- tag anotado: objeto/referencia estable usado para nombrar un hito;
- working tree e index: estado todavía no registrado y selección del próximo commit.

Antes de modificar, `status`, `log --graph --all`, `branch -vv` y `remote -v` deben darte suficiente evidencia para saber qué es local, remoto y compartido.

## Historia B — Entrega revisable

Una dirección razonable crea una branch de feature desde un `main` actualizado, modifica documentación real y separa cambios por intención. Por ejemplo, un commit puede introducir la política/alcance de la candidata y otro actualizar el changelog.

Antes de pedir revisión, una comprobación típica es:

```text
git log --oneline origin/main..HEAD
git diff --check origin/main...HEAD
git diff origin/main...HEAD
```

No importa que tus nombres de branch o archivos difieran si la historia es coherente.

## Historia C — Colaboración

Después de que el segundo clone publique un cambio, la solución debe **obtener primero la realidad remota** y comparar ambos lados. Un patrón razonable es:

```text
git fetch origin
git log --oneline --left-right --graph HEAD...origin/main
```

Si la feature todavía es privada, puedes reaplicarla sobre `origin/main` con rebase. Si quieres conservar explícitamente la topología o el contexto exige merge, puedes integrarlo con merge. Lo importante es no usar force push para borrar el cambio remoto y comprobar después que ambos aportes siguen presentes.

Ante un conflicto, lee los tres estados y el contexto del archivo. La resolución correcta preserva la intención necesaria, no necesariamente todas las líneas literales.

## Historia D — `bisect` y reflog

Para `bisect`, una solución fuerte define una condición automatizable. Por ejemplo, un archivo de política puede ser “bueno” cuando contiene una línea requerida:

```text
grep -q 'Requiere revisión independiente' docs/release-policy.md
```

Marca un commit conocido como bueno y uno conocido como malo, y deja que `git bisect` reduzca el intervalo. Si usas `git bisect run`, el comando debe devolver 0 para bueno y no-cero para malo. Guarda la salida que identifica el primer commit malo y termina con:

```text
git bisect reset
```

Para reflog, el objetivo es recuperar un commit después de perder su referencia sin haber guardado previamente el SHA. Busca en:

```text
git reflog --all
git show <candidato>
```

Cuando confirmes el objeto correcto, crea una nueva branch o tag que vuelva a hacerlo alcanzable. Reflog es local y expirable; no sustituye un remoto o backup.

## Historia E — Tag, hook y gobierno

Un tag de candidata razonable es anotado y se crea sólo cuando puedes explicar el commit que nombra:

```text
git tag -a v1.0.0-rc1 -m 'ReleaseDesk 1.0.0 RC1'
git show v1.0.0-rc1
git push origin v1.0.0-rc1
```

El hook local puede configurarse mediante `.git/hooks/pre-commit` o un `core.hooksPath` versionable/copiadable en el laboratorio. Debe inspeccionar staged content y devolver estado no-cero cuando encuentre `FORBIDDEN-WIP`.

La evidencia debe incluir **un commit rechazado** y, después de reparar el staged content, un commit aceptado.

El punto conceptual es más importante que el shell exacto: un hook de cliente puede omitirse, no existe automáticamente en otros clones y no gobierna el servidor. CI, branch protection, reviews requeridas y permisos sí forman parte del control compartido.

## Historia F — Secreto falso, firmas y documentación

Usa únicamente:

```text
DEMO_TOKEN=final-assessment-not-a-secret
```

Después de registrarlo y borrarlo en un commit posterior, una comprobación como ésta demuestra que el snapshot histórico sigue existiendo:

```text
git show <commit-que-lo-contenia>:<archivo>
```

Si fuera una credencial real, la primera respuesta es revocar/rotar y tratarla como comprometida. La limpieza de historia reduce exposición residual, pero no vuelve segura la credencial original.

`.gitignore` evita ciertos accidentes con archivos no rastreados; no borra objetos históricos ni protege secretos pegados en archivos que sí se versionan.

Una firma verificable aporta evidencia criptográfica de procedencia del objeto bajo una clave. No demuestra revisión, corrección, seguridad ni permiso para producción.

Fuentes oficiales razonables incluyen documentación de `git-push`, `git-bisect`, `git-reflog`, `githooks`, `git-tag`, `git-verify-commit` o capítulos correspondientes de Pro Git. La nota debe conectar explícitamente la fuente con una decisión tomada.

## Verificación de cierre

Una solución fuerte termina con un repositorio comprendido y trazable:

```text
git status --short
git log --oneline --decorate --graph --all
git branch -vv
git tag --list
git reflog --all
```

No necesitas un grafo idéntico a esta referencia. Sí necesitas poder explicar cada branch/tag importante, qué se publicó, qué se recuperó y por qué no se perdió trabajo ajeno.

## Defensa de entrevista

Una respuesta fuerte cuenta un incidente concreto en secuencia: observación → hipótesis → operación → verificación. Evita recitar comandos sin contexto.

Por ejemplo: “Mi push fue rechazado; hice fetch, comparé `HEAD...origin/main`, vi un commit remoto nuevo, reapliqué mi feature privada sobre esa base, corrí el diff de revisión y publiqué sin force”. Esa explicación demuestra más que decir sólo “sé usar rebase”.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y puntúa evidencia, seguridad y comprensión, no similitud con esta referencia.
