# Lección 14 — Tolera un filesystem que cambia

## Objetivo

Aceptar que el árbol de archivos puede cambiar entre observar una entrada y leer sus metadatos.

## Problema

`exists()` no congela el mundo. Otro proceso puede borrar, mover o reemplazar un archivo una instrucción después. Si ThreadSeek convierte cada cambio normal del filesystem en una excepción fatal, un índice largo será frágil.

## Concepto

Las operaciones de `std::filesystem` que tienen overload con `std::error_code` permiten distinguir fallos esperables durante un scan. ThreadSeek contabiliza `entries_skipped` y continúa cuando una entrada desaparece o deja de ser legible, pero sigue rechazando una raíz inválida porque sin raíz no existe trabajo útil que hacer.

La prueba provoca la condición de carrera deliberadamente: la callback elimina el archivo que acaba de observarse antes de consultar su tamaño. El resultado debe omitir esa entrada, contarla como skipped y terminar con los demás archivos.

## Aplicación real

Logs rotados, carpetas de build, caches y descargas cambian mientras se indexan. La estrategia útil es preservar progreso y diagnóstico, no fingir un snapshot transaccional que el filesystem no ofrece.

## Errores comunes

- patrón check-then-act: comprobar existencia y asumir que seguirá existiendo;
- capturar todas las excepciones y ocultar una raíz inválida;
- reintentar infinitamente una entrada que cambia;
- no distinguir archivos omitidos de archivos descubiertos.

## Ejercicio

Agrega un segundo caso donde un archivo se renombre durante el scan. Decide si debe aparecer con el nombre viejo, nuevo o quedar omitido y documenta por qué.

## Comprobación

Ejecuta CTest varias veces; el caso de mutación debe ser determinista porque la propia callback dispara el cambio.

## Reflexión

¿Qué tendría que cambiar en la arquitectura si el requisito fuera un snapshot consistente del directorio en un instante exacto?

## Siguiente paso

Continúa con [Lección 15 — Perfila antes de optimizar](15-perfila-antes-de-optimizar.md).
