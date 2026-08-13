# Lección 16 — Seguridad, hardening y checkpoint 04

## Qué vas a conseguir
Tratarás archivos, rutas, DOM y respuestas HTTP como fronteras que necesitan límites explícitos. Después integrarás arquitectura, validación y experiencia de usuario en el checkpoint 04.

## Antes de empezar
Ejecuta `npm run verify` y `npm start`. Revisa `storage.js`, `board.js`, `server.mjs` y el workflow del curso.

## El problema
Una aplicación local también procesa datos no confiables. Un archivo importado puede ser enorme o inválido. Una ruta HTTP puede intentar escapar del directorio permitido. Un título puede contener texto que sería peligroso si se insertara como HTML. Un servidor puede omitir headers que reducen superficie de ataque.

Seguridad no empieza cuando publicas en Internet; empieza cuando reconoces una frontera.

## Concepto
Kanban Local usa varias defensas pequeñas y complementarias:

### Límite antes de leer
`assertImportFileSize` rechaza archivos mayores a 1,000,000 bytes **antes** de ejecutar `file.text()`. El límite no convierte el contenido en confiable; sólo evita aceptar consumo de memoria sin límite.

### Validación después de parsear
`importBoard` valida JSON, versión y estructura. Después `assertValidBoard` exige ids únicos, títulos normalizados y columnas conocidas.

### Texto, no HTML
Los títulos se renderizan mediante `textContent`. Una cadena como `<img onerror=...>` se presenta como texto; no se interpreta como markup.

### Contención de rutas
El servidor resuelve la ruta solicitada y comprueba con `relative()` que el archivo siga dentro del root permitido. No confía en una comparación textual ingenua de prefijos.

### Headers defensivos
El servidor local emite, entre otros, Content-Security-Policy, `X-Content-Type-Options: nosniff`, política de permisos, referrer policy y protección contra framing. El manifest se sirve con `application/manifest+json`.

Estas defensas no convierten el pequeño servidor educativo en una plataforma de producción. Sí enseñan una idea transferible: **validar en la frontera y reducir capacidades por defecto**.

## Demostración
[EJECUTAR]

```powershell
npm run verify
npm start
```

En otra terminal:

```powershell
curl.exe -I http://127.0.0.1:4173/
```

Revisa los headers. Después ejecuta las pruebas de `hardening.test.js`, que cubren el límite de importación y contenido no válido.

[DEMO] Importa un tablero cuyo título contenga caracteres `<` y `>`. Observa que se muestran literalmente en la interfaz porque usamos `textContent`.

## Código real
La secuencia de importación tiene tres fronteras:

```text
File.size → file.text() → importBoard() → persistencia
```

Cada paso responde una pregunta distinta:

1. ¿aceptamos siquiera leer este tamaño?
2. ¿el texto cumple el formato esperado?
3. ¿el estado resultante satisface las invariantes del dominio?

No combines esas preguntas en un único `try/catch` mental llamado “validación”.

## Qué acaba de pasar
El hardening del curso ya no depende sólo de “tener cuidado”: existen límites ejecutables y CI comprueba que el servidor real devuelve headers importantes.

También evitamos una trampa frecuente: una CSP no sustituye la codificación segura del DOM, igual que un límite de archivo no sustituye validar su estructura. Las capas de defensa resuelven fallos diferentes.

## Errores comunes
- usar `innerHTML` con texto proporcionado por el usuario;
- leer archivos completos antes de comprobar tamaño;
- aceptar cualquier objeto que `JSON.parse` produzca;
- validar rutas con `startsWith(root)` sin considerar límites de directorio;
- copiar headers “de seguridad” sin entender qué capacidad restringen;
- afirmar que un servidor didáctico está listo para Internet sólo porque tiene CSP.

## Buenas prácticas
Mantén límites explícitos, errores comprensibles y defaults restrictivos. Prueba las defensas importantes. Cuando una defensa dependa del navegador o del despliegue real, dilo y conserva una comprobación manual apropiada.

## Tu turno — Checkpoint 04
Resuelve [`../exercises/importacion-segura-04.md`](../exercises/importacion-segura-04.md) sin abrir la solución.

Hoy seleccionar un archivo válido reemplaza inmediatamente el tablero. Cambiarás ese flujo para **previsualizar y pedir confirmación antes de persistir**. La cancelación debe dejar intacto el tablero actual. La vista previa no debe insertar HTML del archivo ni crear una segunda fuente de reglas de negocio.

[PAUSA PARA EJERCICIO]

## Cómo comprobar
Tu solución debe conservar `npm run verify` verde y añadir pruebas para la lógica pura que introduzcas. Verifica manualmente cuatro caminos: importar y aceptar, importar y cancelar, archivo inválido y archivo demasiado grande.

## Solución
Sólo después de intentarlo consulta [`../solutions/importacion-segura-04.md`](../solutions/importacion-segura-04.md).

## Reto adicional
¿Qué cambiaría si el archivo pudiera contener decenas de megabytes y ya no fuera razonable cargarlo completo en memoria? Describe la arquitectura; no agregues una dependencia sólo para responder.

## Resumen
- cualquier entrada externa merece límites explícitos;
- tamaño, sintaxis e invariantes son validaciones diferentes;
- `textContent` mantiene datos como texto;
- las rutas deben permanecer dentro de su raíz real;
- headers defensivos reducen capacidades, pero no sustituyen código seguro;
- checkpoint 04 integra diseño, UX, pruebas y seguridad.

## Siguiente paso
Continúa con la [Lección 17 — Evaluación final sin receta](17-evaluacion-final.md): tendrás que leer arquitectura existente, modificar comportamiento, corregir un defecto, escribir pruebas y justificar decisiones sin una receta paso a paso.

## Referencias
- [Content Security Policy — MDN](https://developer.mozilla.org/docs/Web/HTTP/CSP)
- [`textContent` — MDN](https://developer.mozilla.org/docs/Web/API/Node/textContent)
- [`path.relative()` — Node.js](https://nodejs.org/api/path.html#pathrelativefrom-to)
