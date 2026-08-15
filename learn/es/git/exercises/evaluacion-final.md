# Evaluación final — Opera ReleaseDesk sin receta

Trabaja sobre tu repositorio ReleaseDesk. No abras la solución hasta completar un intento. Puedes consultar documentación oficial y las lecciones, pero nadie te dará una secuencia exacta de comandos.

## Historia A — Lee antes de tocar

Entrega una explicación breve del estado inicial que incluya:

- qué commit señala `HEAD`;
- qué relación existe entre tu branch actual y `origin/main`;
- qué branches y tags existen;
- qué cambios están sin registrar, staged o limpios;
- qué parte del grafo considerarías historia compartida y cuál trabajo privado.

Adjunta evidencia de `status`, `log --graph`, `branch -vv`, `remote -v` y `tag --list`.

## Historia B — Prepara una entrega revisable

ReleaseDesk necesita documentar una entrega candidata `1.0`.

Agrega o modifica documentación real del proyecto para incluir:

- alcance de la entrega;
- responsable de validación;
- criterio explícito de rollback;
- entrada correspondiente en el changelog.

Haz el trabajo en una branch propia y construye **al menos dos commits coherentes**. No se evalúa un nombre exacto de branch ni un texto exacto; se evalúa que staging y commits expresen unidades revisables y que no arrastren cambios accidentales.

Antes de considerar la branch lista, demuestra qué commits y diff ofrecerías a revisión respecto de `origin/main`.

## Historia C — Colaboración sin pisar a otro

Usa un segundo clone para representar a otra persona. Mientras tu trabajo sigue activo, ese colaborador debe publicar a `main` un cambio válido e independiente.

Tu responsabilidad es integrar el nuevo estado remoto y conservar **ambos aportes**.

La evidencia debe demostrar:

- que detectaste la divergencia antes de modificar historia compartida;
- que un eventual rechazo non-fast-forward se entendió como protección, no como obstáculo;
- que no usaste `push --force` ni `--force-with-lease` para imponerte;
- que cualquier conflicto se resolvió por intención y no borrando sin explicación un aporte;
- que la branch final vuelve a estar basada en un estado remoto que puedes explicar.

Puedes elegir merge o rebase para tu branch privada, pero debes justificar la elección.

## Historia D — Encuentra una regresión y recupera trabajo perdido

En una branch descartable de diagnóstico, crea una historia de al menos cinco commits sobre un archivo de política de ReleaseDesk. El primer estado debe ser bueno; exactamente uno de los commits intermedios debe introducir una condición objetivamente mala que puedas comprobar con un comando reproducible. Los commits posteriores no deben reparar esa condición.

Después:

1. define un commit bueno y uno malo;
2. usa `git bisect` para localizar el **primer commit malo**;
3. conserva evidencia de la búsqueda;
4. termina la sesión de bisect correctamente.

No vale escoger el culpable leyendo los mensajes y después simular la búsqueda.

A continuación crea otra branch temporal, haz en ella un commit útil, vuelve a otra branch y elimina la referencia temporal. **No copies el SHA antes de eliminarla.** Recupera el commit usando evidencia de reflog y vuelve a crear una referencia segura hacia él.

Explica por qué reflog te ayudó y por qué no deberías tratarlo como backup permanente.

## Historia E — Versiona una candidata y demuestra gobierno local/compartido

Cuando tu entrega candidata esté en un estado que puedas defender:

- crea un tag **anotado** `v1.0.0-rc1` sobre el commit correcto;
- demuestra qué commit representa y publica sólo la referencia que necesitas;
- instala un hook local `pre-commit` que rechace staged content con la cadena `FORBIDDEN-WIP`;
- demuestra un rechazo real del hook y después un commit válido;
- explica por escrito por qué ese hook no puede imponer por sí solo la política a todo un equipo.

Propón qué controles moverías o duplicarías en CI/plataforma para gobernar una organización: reviews, checks requeridos, protección de branches, permisos, secret scanning u otros controles justificados.

## Historia F — Incidente de seguridad y documentación oficial

Usa exclusivamente este valor falso:

```text
DEMO_TOKEN=final-assessment-not-a-secret
```

En una branch descartable, registra el archivo de laboratorio en un commit y elimínalo en el commit siguiente. Demuestra que el snapshot histórico todavía permite recuperar el valor falso.

Después responde:

1. ¿Qué harías primero si el valor hubiera sido una credencial real?
2. ¿Por qué reescribir historia no vuelve confiable una credencial ya expuesta?
3. ¿Qué riesgo reduce `.gitignore` y cuál no?
4. ¿Qué demuestra una firma verificable y qué **no** demuestra?

Consulta al menos **dos fuentes oficiales de Git** relacionadas con decisiones tomadas durante la evaluación. Para cada una entrega:

- enlace;
- qué verificaste;
- qué decisión respaldó.

## Entrega

Entrega:

- el repositorio o bundle de evidencia acordado;
- grafo final de historia/referencias;
- comandos de verificación relevantes y sus resultados;
- explicación de un conflicto o rechazo que encontraste y cómo lo diagnosticastes;
- evidencia de `bisect` y recuperación por reflog;
- evidencia del tag anotado;
- evidencia del hook local;
- respuesta al incidente de seguridad;
- las dos notas de documentación oficial;
- una explicación de 250–400 palabras de tu estrategia de colaboración y recuperación.

## Restricciones

- No uses credenciales, tokens o secretos reales.
- No uses `git push --force` ni `--force-with-lease` sobre la historia compartida del escenario.
- No uses `git reset --hard` para ocultar un estado que no entiendes.
- No borres y recrees clones para evitar diagnosticar divergencia.
- No copies el SHA del commit que debes recuperar antes de borrar su branch.
- No debilites un hook, validación o comprobación sólo para conseguir un estado verde.

## Comprobación mínima final

Debes poder explicar la salida de:

```text
git status --short
git log --oneline --decorate --graph --all
git branch -vv
git remote -v
git tag --list
git show v1.0.0-rc1
git reflog --all
git config --get core.hooksPath
```

El repositorio principal debe terminar en un estado limpio o con cualquier excepción documentada explícitamente.

Evalúate con [`rubrica-final.md`](rubrica-final.md).
