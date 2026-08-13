# Checkpoint 04 — Diagnosticar, recuperar y endurecer sin destruir evidencia

Resuelve este checkpoint sin abrir la solución. Usa únicamente datos de laboratorio. **No introduzcas credenciales ni secretos reales.**

## Escenario

ReleaseDesk sufrió un incidente compuesto:

- una comprobación empezó a fallar en algún punto de una serie de commits;
- una branch con una nota importante fue eliminada antes de integrarse;
- el equipo quiere feedback local para impedir una marca WIP concreta;
- alguien demuestra, con un valor falso, que borrar un archivo sensible del working tree no lo elimina de la historia.

Tu trabajo es reconstruir evidencia y proponer controles sin usar fuerza bruta.

## Parte A — Encuentra la regresión

1. Crea una branch `incident/bisect` desde `main`.
2. Crea `docs/health.txt` con `status=healthy` y haz un commit que puedas validar como bueno.
3. Genera al menos cuatro commits posteriores.
4. En exactamente uno cambia la primera línea a `status=broken` y no la repares en los commits siguientes.
5. Usa `git log` limitado al archivo y `git blame` para obtener contexto.
6. Usa `git bisect` con un punto bueno conocido y un punto malo conocido para identificar el **primer commit malo**.
7. Ejecuta `git bisect reset` antes de continuar.

No se vale identificar el SHA leyendo de antemano el historial de comandos con el que creaste la regresión.

## Parte B — Recupera una referencia perdida

1. Desde `main`, crea `recovery/checkpoint-04`.
2. Agrega `docs/recovery-note.md` con contenido único y haz commit con un mensaje reconocible.
3. Vuelve a `main` y elimina deliberadamente esa branch de laboratorio.
4. Usa `git reflog --all` para localizar el commit.
5. Inspecciónalo con `git show` **antes** de recuperarlo.
6. Créale una nueva referencia `recovered/checkpoint-04` y demuestra que el archivo está ahí.

No uses un SHA copiado antes de borrar la branch como mecanismo principal de recuperación.

## Parte C — Feedback local con alcance honesto

1. Crea una branch `policy/checkpoint-04`.
2. Configura un `pre-commit` local mediante `core.hooksPath`.
3. El hook debe ejecutar `git diff --cached --check` y rechazar una línea staged que contenga `FORBIDDEN-WIP`.
4. Demuestra un intento de commit rechazado.
5. Repara el contenido y demuestra un commit aceptado.
6. Explica por qué ese hook **no basta** para gobernar una organización.

No conviertas el hook en la única defensa de una política crítica.

## Parte D — Secreto falso e historia

Usa exactamente un valor de demostración equivalente a:

```text
DEMO_TOKEN=example-only-not-a-secret
```

1. En una branch descartable agrega un archivo con ese valor falso y haz commit.
2. En el commit siguiente elimina el archivo.
3. Demuestra que el working tree ya no lo contiene.
4. Usa `git show <sha>:<ruta>` para demostrar que el commit anterior todavía conserva el valor falso.
5. **No reescribas historia como parte obligatoria del checkpoint.** Explica qué harías si hubiera sido una credencial real.

## Restricciones

- No uses secretos reales.
- No uses `git push --force` ni `--force-with-lease`.
- No uses `git reset --hard` para resolver el incidente.
- No ejecutes limpieza agresiva de objetos (`git gc`, `git prune`) durante la recuperación.
- No atribuyas culpabilidad humana con `git blame`; úsalo sólo como evidencia de procedencia.
- Deja `main` y tu working tree principal limpios al terminar.

## Evidencia mínima

Incluye salidas o explicaciones verificables de:

```text
git log --oneline -- docs/health.txt
git blame -L 1,1 docs/health.txt
git bisect log
git reflog --all
git show recovered/checkpoint-04:docs/recovery-note.md
git config --get core.hooksPath
git status --short
```

Además entrega:

- el SHA del primer commit malo identificado por `bisect`;
- la salida del intento rechazado por el hook;
- la salida de `git show` que demuestra persistencia del secreto **falso** en historia;
- una propuesta de control compartido equivalente al hook.

## Preguntas de reflexión

1. ¿Qué respondió `log` que `blame` no respondía, y viceversa?
2. ¿Por qué `bisect` necesitaba un punto bueno realmente verificado?
3. ¿Qué información local conservó reflog después de eliminar la branch?
4. ¿Por qué un hook local mejora velocidad de feedback pero no basta como política de integración?
5. Si el token hubiera sido real, ¿por qué borrarlo y reescribir Git no sustituiría rotarlo o revocarlo?
6. ¿Qué diferencia existe entre un commit firmado, un commit revisado y un commit autorizado para entrar a `main`?

## Criterio de éxito

Apruebas el checkpoint si puedes reconstruir el incidente con evidencia, recuperar la referencia sin destruir información, demostrar el alcance limitado del hook y explicar correctamente la respuesta ante un secreto real sin haber usado ninguno.
