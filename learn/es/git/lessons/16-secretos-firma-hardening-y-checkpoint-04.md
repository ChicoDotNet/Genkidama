# Lección 16 — Secretos, firma, hardening y checkpoint 04

## Qué vas a conseguir

Demostrarás por qué borrar un secreto del working tree no lo elimina de la historia, distinguirás procedencia criptográfica de permisos de integración y cerrarás el cuarto checkpoint con diagnóstico, recuperación, automatización y seguridad.

## Antes de empezar

Completa la [Lección 15](15-hooks-automatizacion-y-politicas.md).

## El problema

Un archivo sensible entra por error en un commit. En el siguiente commit lo borras y `git status` queda limpio. Parece resuelto, pero alguien que conoce el commit anterior todavía puede leerlo.

Al mismo tiempo, tu equipo quiere saber quién produjo ciertos commits y evitar que cambios no revisados lleguen a `main`.

Son problemas diferentes: **confidencialidad**, **procedencia** y **gobierno de integración**.

## Concepto — un secreto comprometido sigue comprometido

En esta lección usa **únicamente un valor falso de laboratorio**. Nunca practiques con credenciales reales.

Crea una branch descartable:

```text
git switch -c security/secret-demo main
```

Agrega `demo-secret.txt`:

```text
DEMO_TOKEN=example-only-not-a-secret
```

Haz commit y anota su SHA. Luego elimina el archivo y haz otro commit.

Tu working tree ya no contiene el archivo, pero prueba:

```text
git show <SHA_DEL_COMMIT>:demo-secret.txt
```

El valor falso sigue en el objeto histórico.

## Respuesta correcta ante un secreto real

Si esto ocurriera con una credencial real, el orden importa:

1. **revoca o rota el secreto**; asume que dejó de ser confiable;
2. detén propagación adicional y avisa a quien administra el sistema afectado;
3. elimina el secreto del estado actual;
4. evalúa si la política exige reescribir historia y coordina esa operación con el equipo/plataforma;
5. revisa clones, artefactos, logs, caches y otros lugares donde pudo copiarse;
6. agrega prevención: secret scanning, `.gitignore` adecuado, gestores de secretos, CI y revisión.

Reescribir Git no “descompromete” una credencial ya expuesta. La rotación/revocación es la acción primaria.

## `.gitignore` no es un vault

Ignorar `.env` o archivos equivalentes reduce accidentes con archivos no rastreados, pero no protege:

- secretos ya comprometidos en commits;
- secretos pegados en archivos que sí deben versionarse;
- secretos incluidos en logs o artefactos;
- credenciales copiadas fuera del repositorio.

La prevención real combina diseño, almacenamiento seguro y controles automáticos.

## Firma y procedencia

Git puede firmar commits y tags mediante mecanismos soportados por tu entorno, por ejemplo GPG o SSH. Una firma verificable aporta evidencia criptográfica sobre procedencia del objeto firmado.

Comandos de inspección:

```text
git log --show-signature
git verify-commit <sha>
git verify-tag <tag>
```

La configuración de claves depende del entorno y no es requisito para completar ReleaseDesk. Lo importante es distinguir qué responde una firma: **quién controla una clave asociada al objeto**, no si el cambio era correcto, seguro o autorizado para producción.

## Hardening de integración

Una plataforma de repositorios puede complementar Git con:

- protección de `main`;
- revisiones requeridas;
- checks de CI obligatorios;
- restricciones de force push;
- permisos mínimos necesarios;
- secret scanning;
- reglas sobre commits/tags firmados cuando el contexto lo justifique.

Ningún control aislado sustituye los demás.

## Qué acaba de pasar

La historia distribuida es precisamente lo que hace a Git útil para trazabilidad, pero también significa que datos sensibles versionados pueden persistir en objetos y copias. La solución no es “borrar más fuerte”; es tratar el secreto como comprometido y gobernar cómo entra información al repositorio.

## Errores comunes

- Borrar un secreto en el siguiente commit y dar por cerrado el incidente.
- Poner un secreto real en un ejercicio para “demostrar” el problema.
- Asumir que `.gitignore` afecta archivos ya rastreados.
- Confundir commit firmado con commit aprobado o seguro.
- Usar force push como primera respuesta a un incidente sin coordinación.
- Confiar únicamente en un hook local para impedir secretos.

## Buenas prácticas

Antes de publicar:

```text
git status --short
git diff --staged
git diff --staged --check
```

En equipos, complementa la revisión humana con CI y capacidades de seguridad de la plataforma. Mantén secretos fuera del repositorio mediante mecanismos diseñados para secretos.

## Tu turno — Checkpoint 04

Resuelve [`../exercises/checkpoint-04.md`](../exercises/checkpoint-04.md) sin abrir la solución.

El escenario integra:

- diagnóstico con `log`, `blame` y `bisect`;
- recuperación con reflog;
- un hook local que falla temprano;
- persistencia histórica de un **secreto falso**;
- explicación de qué controles deben ser compartidos y gobernables.

## Cómo comprobar

Debes poder mostrar evidencia de:

```text
git log --oneline -- <ruta-diagnosticada>
git blame <ruta-diagnosticada>
git reflog --all
git config --get core.hooksPath
git show <sha-de-laboratorio>:<archivo-de-secreto-falso>
git status --short
```

Y explicar qué riesgo resuelve cada control y cuál **no** resuelve.

## Solución

Consulta [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md) sólo después de intentar el incidente completo.

## Reto adicional

Diseña un flujo de respuesta para “secreto real encontrado en una PR ya fusionada”. Separa acciones de Git, acciones sobre la credencial, acciones sobre la plataforma y comunicación al equipo. No ejecutes el escenario con un secreto real.

## Resumen

- borrar un archivo no borra sus versiones históricas;
- un secreto real expuesto debe rotarse/revocarse;
- `.gitignore` ayuda a prevenir ciertos accidentes, no reemplaza un gestor de secretos;
- firmas aportan procedencia, no calidad ni autorización por sí solas;
- hardening combina controles locales, CI y gobierno de integración.

## Siguiente paso

Continúa con [Lección 17 — Evaluación final Git Junior sin receta](17-evaluacion-final.md).

## Referencias

- [`git-show`](https://git-scm.com/docs/git-show)
- [`git-verify-commit`](https://git-scm.com/docs/git-verify-commit)
- [`git-verify-tag`](https://git-scm.com/docs/git-verify-tag)
- [Signing Your Work — Pro Git](https://git-scm.com/book/en/v2/Git-Tools-Signing-Your-Work)
- [Removing sensitive data from a repository — GitHub Docs](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/removing-sensitive-data-from-a-repository)
