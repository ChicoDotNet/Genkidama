# Evaluación final — BackupForge

Resuelve este encargo sin una receta de archivos o funciones. Puedes consultar documentación oficial y las lecciones, pero no abras la solución hasta completar un intento serio.

## Historia A — Restauración selectiva

El equipo necesita restaurar **un solo archivo** de un backup o snapshot sin copiar todo el árbol. La operación debe recibir una ruta relativa segura y producir el archivo en el destino conservando su estructura relativa.

Antes de escribir bytes debes decidir y documentar qué evidencia verificas. Una solución fuerte conserva la garantía actual de no restaurar desde un backup que ya se sabe inconsistente.

Escribe primero pruebas que protejan:

- restauración correcta de una ruta existente;
- rechazo de una ruta que no pertenece al manifest;
- rechazo de un backup corrupto antes de publicar el archivo restaurado.

## Historia B — Bug de rutas equivalentes

El validador actual distingue strings, pero una ruta como `docs/./manual.txt` puede referirse al mismo destino que `docs/manual.txt`. Corrige la validación para que un manifest no pueda contener componentes de ruta redundantes o ambiguos que terminen en el mismo lugar.

Añade una regresión que hubiera fallado antes del arreglo. No resuelvas el problema sólo en la CLI: el contrato pertenece a la frontera que acepta manifests.

## Historia C — Errores y API pública

Conserva los contratos existentes:

- errores recuperables se propagan mediante `Result`;
- la biblioteca no llama `process::exit` ni imprime como mecanismo de control;
- una ruta insegura produce un error explícito;
- una restauración fallida no debe presentarse como éxito parcial.

Si agregas API pública, documenta propósito, parámetros, retorno, errores y efectos de I/O con rustdoc.

## Historia D — Regresión y tooling

Tu solución debe dejar verde el gate completo:

```bash
bash tools/verify.sh
```

No desactives Clippy, tests o formatter para conseguir verde. Si el compilador o Clippy te muestran un error, úsalo como evidencia y documenta brevemente qué cambiaste.

## Historia E — Documentación oficial

Consulta al menos dos fuentes oficiales de Rust y deja una nota breve con la decisión que sustentan. Una debe relacionarse con `std::path::Component`, filesystem o `Result`; la otra puede cubrir Cargo, testing, rustdoc o errores.

## Historia F — Diseño de mejora

Sin implementarlo, diseña el siguiente paso para almacenar snapshots en un repositorio remoto con múltiples escritores. Identifica:

- qué frontera sustituirías primero;
- cómo publicarías una versión sin exponer estado parcial;
- cómo evitarías colisiones de nombres o escrituras perdidas;
- cómo autenticarías manifests además de detectar corrupción accidental;
- qué observabilidad necesitarías sin registrar nombres/contenido sensible innecesario.

## Evidencia mínima

Entrega:

```bash
bash tools/verify.sh
```

Además muestra:

1. prueba de restauración selectiva válida;
2. prueba de ruta no declarada o insegura;
3. regresión para una ruta con componente `.`;
4. prueba de backup corrupto que no publica el archivo restaurado;
5. dos referencias oficiales consultadas;
6. una defensa de arquitectura de aproximadamente cinco minutos.

Autoevalúate con [`rubrica-final.md`](rubrica-final.md).
