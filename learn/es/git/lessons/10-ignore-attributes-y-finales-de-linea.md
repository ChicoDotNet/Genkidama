# Lección 10 — `.gitignore`, `.gitattributes` y finales de línea

## Qué vas a conseguir

Controlarás qué archivos no deben entrar al repositorio y qué reglas de texto debe aplicar Git, evitando que Windows y Linux conviertan un cambio pequeño en un diff lleno de CRLF/LF.

## Antes de empezar

Completa la [Lección 09](09-tags-y-releases.md).

## El problema

ReleaseDesk empieza a generar archivos temporales y puede ser editado desde sistemas distintos. Dos riesgos aparecen rápido:

1. terminar versionando basura generada localmente;
2. producir cambios masivos sólo por diferencias de finales de línea.

## Concepto

`.gitignore` describe rutas no rastreadas que Git debe ignorar. No borra ni deja de seguir automáticamente archivos que ya estaban versionados.

`.gitattributes` describe propiedades que Git aplica a rutas versionadas. Para texto, puede declarar normalización y el final de línea esperado en el working tree.

Ejemplo:

```text
* text=auto
*.md text eol=lf
*.ps1 text eol=crlf
```

Esto no significa que todo sistema almacene internamente el archivo de la misma forma en el working tree. El objetivo es que el repositorio tenga una política explícita y que Git pueda normalizar contenido de manera reproducible.

## Demostración

ReleaseDesk ya ignora:

```text
tmp/
*.log
```

Comprueba:

```text
mkdir -p tmp
printf "diagnóstico\n" > tmp/debug.log
git status --short
git check-ignore -v tmp/debug.log
```

El archivo no debe aparecer como candidato normal al commit.

Crea `.gitattributes`:

```text
* text=auto
*.md text eol=lf
*.ps1 text eol=crlf
```

Después:

```text
git add .gitattributes
git commit -m "chore: definir atributos de texto"
git check-attr text eol -- README.md
```

## Código real

Crea `scripts/release.ps1` y comprueba sus atributos:

```text
git check-attr text eol -- scripts/release.ps1
```

Git debe reportar `text: set` y `eol: crlf` para ese patrón.

Antes de registrar cambios de normalización en un repositorio existente, inspecciona cuidadosamente el diff. Agregar `.gitattributes` puede revelar diferencias que antes estaban ocultas por configuración local.

## Errores comunes

- Agregar un archivo ya rastreado a `.gitignore` y esperar que desaparezca del índice.
- Configurar `core.autocrlf` a ciegas y asumir que sustituye una política de repositorio.
- Mezclar cambios funcionales con una normalización masiva de EOL.
- Usar `.gitignore` como sustituto de no crear archivos sensibles en el repositorio.

## Buenas prácticas

Consulta qué regla aplica realmente:

```text
git check-ignore -v ruta
git check-attr -a -- ruta
```

Cuando cambies atributos de texto en un proyecto existente, hazlo como un cambio aislado y revisable.

## Tu turno

Agrega una regla para ignorar una carpeta `artifacts/` y una regla de atributos para que archivos `.sh` usen LF. Demuestra ambas reglas con `git check-ignore` y `git check-attr` antes de hacer commit.

## Cómo comprobar

```text
git check-ignore -v artifacts/demo.txt
git check-attr text eol -- scripts/demo.sh
git diff --check
```

## Reto adicional

Crea una branch descartable y experimenta con un archivo de texto que cambie entre CRLF y LF. Usa `git diff --word-diff` y `git check-attr` para distinguir un cambio real de contenido de un cambio de representación.

## Resumen

- `.gitignore` controla archivos no rastreados, no desversiona archivos existentes;
- `.gitattributes` permite una política compartida de tratamiento de archivos;
- EOL debe ser una decisión explícita del repositorio cuando importa;
- `check-ignore` y `check-attr` explican por qué Git está actuando de cierta forma.

## Siguiente paso

Continúa con [sincronización colaborativa segura](11-sincronizacion-colaborativa-segura.md).

## Referencias

- [`gitignore`](https://git-scm.com/docs/gitignore)
- [`gitattributes`](https://git-scm.com/docs/gitattributes)
- [`git-check-ignore`](https://git-scm.com/docs/git-check-ignore)
- [`git-check-attr`](https://git-scm.com/docs/git-check-attr)
