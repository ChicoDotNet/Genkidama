# Curso de COBOL desde cero — Construye un procesador batch de nómina

Aprende COBOL desde cero construyendo **NominaBatch**, una aplicación local que procesa registros ficticios de empleados, valida datos, calcula importes y genera un reporte reproducible.

El curso usa **GnuCOBOL 3.2.0** validado en Ubuntu 24.04 mediante GitHub Actions. Enseña fundamentos transferibles del lenguaje y no simula experiencia en z/OS, CICS, JCL ni productos comerciales. COBOL conserva uso en sistemas empresariales de larga vida, pero su mercado es más especializado que el de lenguajes web generalistas; este curso prepara práctica demostrable y no promete empleo.

## Tooling verificado

- GnuCOBOL 3.2.0.
- Ubuntu 24.04 en CI; Windows 11 o Linux actual como objetivo local.
- VS Code opcional; no se requiere IDE comercial.

Comprueba tu instalación con `cobc --version`.

## Build, Test y Run

Desde `app/`:

```text
cobc -x -free -Wall -I copybooks -o nomina src/nomina.cob
bash tools/verify.sh
./nomina
```

En Windows el ejecutable generado será `nomina.exe`. La entrada canónica vive en `data/employees.dat` y el resultado en `report.txt`.

El workflow `.github/workflows/learn-cobol.yml` ejecuta `tests/smoke.sh`; ese smoke incluye resultados de negocio y escenarios operativos. `tools/verify.sh` es el wrapper local equivalente.

## Ruta del curso

Estado: **17 de 17 lecciones implementadas — curso completo**.

1. [Tu primer batch COBOL](lessons/01-tu-primer-batch-cobol.md)
2. [Datos `PIC` y registros](lessons/02-datos-pic-y-registros.md)
3. [Validación y aritmética decimal](lessons/03-validacion-y-calculo-nomina.md)
4. [Archivos, procedimientos y checkpoint 01](lessons/04-archivos-procedimientos-y-checkpoint.md)
5. [Copybooks y contratos de datos](lessons/05-copybooks.md)
6. [`FILE STATUS` y fallos explícitos](lessons/06-file-status.md)
7. [Totales de control y reconciliación](lessons/07-totales-control.md)
8. [Pruebas de regresión y checkpoint 02](lessons/08-pruebas-regresion-y-checkpoint.md)
9. [Tablas `OCCURS` para resumir el lote](lessons/09-tablas-occurs.md)
10. [Recorrer tablas con `PERFORM VARYING`](lessons/10-perform-varying-y-reportes.md)
11. [Buscar IDs y proteger la integridad del lote](lessons/11-busqueda-ids-duplicados.md)
12. [Límites, reconciliación y checkpoint 03](lessons/12-limites-reconciliacion-y-checkpoint.md)
13. [Organización y fronteras del programa](lessons/13-organizacion-y-fronteras.md)
14. [Tooling y gate profesional](lessons/14-tooling-y-gate-profesional.md)
15. [Diagnóstico y rendimiento con evidencia](lessons/15-diagnostico-y-rendimiento.md)
16. [Operación confiable y checkpoint 04](lessons/16-operacion-confiable-y-checkpoint-04.md)
17. [Evaluación final COBOL sin receta](lessons/17-evaluacion-final.md)

## Checkpoints y evaluación

- [Checkpoint 01 — Regla de horas extra](exercises/checkpoint-01.md) · [solución](solutions/checkpoint-01.md)
- [Checkpoint 02 — Reconciliación de registros](exercises/checkpoint-02.md) · [solución](solutions/checkpoint-02.md)
- [Checkpoint 03 — Reconciliar agregados por banda](exercises/checkpoint-03.md) · [solución](solutions/checkpoint-03.md)
- [Checkpoint 04 — Invariantes operativas](exercises/checkpoint-04.md) · [solución](solutions/checkpoint-04.md)
- [Evaluación final](exercises/evaluacion-final.md) · [rúbrica](exercises/rubrica-final.md) · [solución de referencia](solutions/evaluacion-final.md)

## Qué sabrás hacer al terminar

Leer y escribir COBOL sencillo; modelar datos con `PIC` y `OCCURS`; recorrer tablas pequeñas; trabajar con archivos; implementar reglas deterministas; separar parsing, validación, cálculo y reporting; compilar con `cobc`; construir gates reproducibles; diagnosticar fallos por contratos observables; razonar sobre rendimiento con evidencia; modificar una base existente y explicar arquitectura y límites en una entrevista junior.

## Cómo hablar de este proyecto en una entrevista

Cuenta el flujo de negocio antes de la sintaxis: entrada batch → validación → cálculo decimal → reporte → controles → pruebas. Explica por qué `FILE STATUS` y los códigos de retorno hacen operable un batch, por qué el copybook es un contrato de datos, cómo `OCCURS` modela agregados fijos y por qué un ID duplicado se rechaza antes de afectar cifras. Menciona que el smoke cubre camino funcional y escenarios operativos reproducibles. Distingue GnuCOBOL local de un entorno mainframe real y no afirmes experiencia que el proyecto no demuestra.

Preguntas probables:

- ¿Por qué un registro inválido no debe modificar acumuladores?
- ¿Qué aporta `FILE STATUS` a la operación de un batch?
- ¿Cómo modelas precisión decimal con `PIC`?
- ¿Cuándo dejaría de ser razonable una búsqueda lineal de IDs?
- ¿Qué demuestra NominaBatch y qué no demuestra sobre z/OS o CICS?

## FAQ

**¿Necesito un mainframe?** No para aprender fundamentos del lenguaje. NominaBatch funciona localmente con GnuCOBOL.

**¿Por qué GnuCOBOL?** Porque es libre, portable y permite practicar COBOL sin depender de una cuenta o servicio comercial.

**¿Por qué formato libre?** Reduce fricción de columnas durante el aprendizaje, sin negar que mucho código COBOL histórico usa formato fijo.

**¿Las pruebas actuales miden cobertura de líneas?** No. Los gates validan comportamientos concretos de punta a punta; no se publica un porcentaje de cobertura no medido.

**¿La tabla de IDs escala sin límite?** No. Esta versión acepta hasta 100 IDs por lote y lo documenta explícitamente.

**¿Por qué no optimizar ya la búsqueda de duplicados?** Porque el contrato actual está acotado a 100 IDs. Si el volumen cambia, primero se mide y después se elige otra estructura.

## Glosario inicial

- **DIVISION:** gran sección estructural de un programa COBOL.
- **PIC / PICTURE:** descripción de forma y capacidad de un dato.
- **V:** posición decimal implícita en una `PICTURE` numérica.
- **FILE SECTION:** descripción de registros asociados a archivos.
- **LINE SEQUENTIAL:** organización donde cada registro corresponde a una línea.
- **paragraph:** bloque nombrado de `PROCEDURE DIVISION` ejecutable con `PERFORM`.
- **copybook:** contrato de código/datos incorporado mediante `COPY` durante compilación.
- **FILE STATUS:** resultado de dos caracteres asociado a operaciones de archivo.
- **total de control:** acumulador usado para reconciliar el conjunto procesado.
- **OCCURS:** cláusula para declarar elementos repetidos de una estructura.
- **PERFORM VARYING:** iteración controlada con variable, incremento y condición de término.
- **código de retorno:** valor entregado al sistema operativo para distinguir éxito de clases de fallo.
- **invariancia:** condición que debe permanecer verdadera independientemente del fixture concreto.

## Referencias oficiales

- [GnuCOBOL](https://gnucobol.sourceforge.io/)
- [GnuCOBOL 3.2](https://sourceforge.net/projects/gnucobol/files/gnucobol/3.2/)
- [Manual de GnuCOBOL](https://gnucobol.sourceforge.io/doc/gnucobol.html)
- [Guías de GnuCOBOL](https://gnucobol.sourceforge.io/guides.html)

## Siguiente paso

Empieza con la [Lección 1](lessons/01-tu-primer-batch-cobol.md). Al terminar las 17 lecciones y cuatro checkpoints, resuelve la [evaluación final](exercises/evaluacion-final.md) sin abrir primero la solución de referencia.
