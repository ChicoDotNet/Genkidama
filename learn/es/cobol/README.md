# Curso de COBOL desde cero — Construye un procesador batch de nómina

Aprende COBOL desde cero construyendo **NominaBatch**, una aplicación local que procesa registros de empleados, valida datos, calcula importes y genera un reporte reproducible.

El curso usa **GnuCOBOL 3.2**. Enseña fundamentos transferibles del lenguaje y no simula experiencia en z/OS, CICS, JCL ni productos comerciales. COBOL conserva uso en sistemas empresariales de larga vida, pero su mercado es más especializado que el de lenguajes web generalistas; este curso prepara práctica demostrable y no promete empleo.

## Tooling verificado

- GnuCOBOL 3.2.
- Windows 11 o Linux actual.
- VS Code opcional; no se requiere IDE comercial.

Comprueba tu instalación con `cobc --version`.

## Build, Test y Run

Desde `app/`:

```text
cobc -x -free -Wall -o nomina src/nomina.cob
bash tests/smoke.sh
./nomina
```

En Windows el ejecutable generado será `nomina.exe`. La entrada canónica vive en `data/employees.dat` y el resultado en `report.txt`.

El CI específico del curso está pendiente de materializarse; no se declara verde hasta que GitHub Actions ejecute realmente el compilador y el smoke test.

## Ruta actual

Estado: **4 de 17 lecciones implementadas**.

1. [Tu primer batch COBOL](lessons/01-tu-primer-batch-cobol.md)
2. [Datos `PIC` y registros](lessons/02-datos-pic-y-registros.md)
3. [Validación y aritmética decimal](lessons/03-validacion-y-calculo-nomina.md)
4. [Archivos, procedimientos y checkpoint 01](lessons/04-archivos-procedimientos-y-checkpoint.md)

## Checkpoint

- [Checkpoint 01 — Regla de horas extra](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)

## Qué sabrás hacer al terminar

Leer y escribir COBOL sencillo; modelar datos con `PIC`; trabajar con archivos; implementar reglas de negocio deterministas; separar parsing, validación, cálculo y reporting; compilar con `cobc`; probar comportamiento; modificar una base existente y explicar arquitectura y límites en una entrevista junior.

## Cómo hablar de este proyecto en una entrevista

Cuenta el flujo de negocio antes de la sintaxis: entrada batch → validación → cálculo decimal → reporte → pruebas. Distingue GnuCOBOL local de un entorno mainframe real y no afirmes experiencia que el proyecto no demuestra.

## FAQ

**¿Necesito un mainframe?** No para aprender fundamentos del lenguaje. NominaBatch funciona localmente con GnuCOBOL.

**¿Por qué GnuCOBOL?** Porque es libre, portable y permite practicar COBOL sin depender de una cuenta o servicio comercial.

**¿Por qué formato libre?** Reduce fricción de columnas durante el aprendizaje, sin negar que mucho código COBOL histórico usa formato fijo.

## Glosario inicial

- **DIVISION:** gran sección estructural de un programa COBOL.
- **PIC / PICTURE:** descripción de forma y capacidad de un dato.
- **V:** posición decimal implícita en una `PICTURE` numérica.
- **FILE SECTION:** descripción de registros asociados a archivos.
- **LINE SEQUENTIAL:** organización donde cada registro corresponde a una línea.
- **paragraph:** bloque nombrado de `PROCEDURE DIVISION` ejecutable con `PERFORM`.

## Referencias oficiales

- [GnuCOBOL](https://gnucobol.sourceforge.io/)
- [GnuCOBOL 3.2](https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/)
- [Manual de GnuCOBOL](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- [Guías de GnuCOBOL](https://gnucobol.sourceforge.io/guides.html)

## Siguiente paso

Empieza con la [Lección 1](lessons/01-tu-primer-batch-cobol.md).
