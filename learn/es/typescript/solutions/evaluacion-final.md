# Solución de referencia — Evaluación final TypeScript

> Abre esta referencia sólo después de completar un intento. No existe una única solución correcta.

## Dirección de diseño

Una solución razonable conserva la fecha objetivo como dato serializable del dominio, por ejemplo `string | undefined`, pero **no confía** en que cualquier `string` sea una fecha válida. La frontera que recibe datos externos normaliza y valida `YYYY-MM-DD`; el dominio puede reutilizar esa misma regla sin depender de HTTP ni DOM.

La regla de “vencido” debe ser determinista: recibe una fecha de referencia explícita y compara proyectos que tengan fecha objetivo, excluyendo los `completed`. No debe leer `new Date()` dentro de la regla porque eso haría las pruebas dependientes del reloj del proceso.

## Historia A — Fecha objetivo

Una dirección posible es definir una función pura que acepte `unknown` y devuelva la representación validada o lance un error claro. Una validación robusta no se limita a una expresión regular: `2026-02-31` tiene la forma correcta, pero no representa una fecha calendario real.

Por ejemplo, la implementación puede:

1. comprobar el patrón `YYYY-MM-DD`;
2. separar año, mes y día;
3. construir una fecha UTC de referencia;
4. verificar que año, mes y día resultantes coinciden exactamente;
5. conservar el texto normalizado como contrato JSON.

Los snapshots antiguos sin la propiedad deben seguir cargando sin migración destructiva.

## Historia B — Vencidos

Una función equivalente a:

```ts
function isOverdue(project: Project, referenceDate: string): boolean
```

puede expresar la regla sin I/O. El nombre no es obligatorio. Lo importante es que:

- `referenceDate` también esté validada;
- `completed` nunca sea vencido;
- la comparación use un formato ordenable o una representación temporal normalizada;
- la consulta devuelva nuevos arrays y no mutaciones del estado.

La API puede exponer la consulta mediante query params u otra ruta coherente con la aplicación, pero el status HTTP pertenece al adaptador.

## Historia C — IDs duplicados

La referencia **rechaza** un ID ya existente. Es una política simple y auditable: el servidor no adivina cuál proyecto debe ganar ni cambia silenciosamente una identidad recibida.

La comprobación debe ocurrir antes de persistir el snapshot candidato. Si el store rechaza la escritura, la memoria visible conserva el snapshot anterior, igual que en las mutaciones durables ya aprendidas.

Una regresión fuerte demuestra tanto el error como la ausencia de mutación/persistencia parcial.

## Historia D — Regresión

Ejecuta:

```bash
npm run verify
npm start
```

No cambies `strict`, los límites HTTP, headers, diagnóstico o contratos de persistencia salvo que tu solución lo requiera realmente. Si amplías una superficie exportada, documenta su contrato con TSDoc/JSDoc conforme a las convenciones del curso.

## Historia E — Documentación y diseño

Una nota válida podría consultar:

- el Handbook de TypeScript para justificar `unknown` + narrowing en datos externos;
- MDN `Date`/`toISOString` para entender normalización temporal;
- Node.js `fs` para razonar sobre la limitación de un archivo local frente a múltiples procesos.

La respuesta de diseño debe reconocer que un archivo JSON con reemplazo atómico local no ofrece control de concurrencia distribuida. Una futura persistencia multiusuario requeriría que el contrato del store tratara versiones, conflictos o transacciones; el dominio no debería acoplarse directamente a SQL o HTTP por esa razón.

## Defensa de entrevista

Una respuesta fuerte distingue cuatro capas:

- **tipos/reglas**: expresan invariantes y comportamiento determinista;
- **orquestación**: construye snapshots candidatos y decide cuándo una mutación es visible;
- **I/O**: HTTP y filesystem traducen contratos externos;
- **presentación**: el navegador captura/muestra datos sin convertirse en autoridad del dominio.

También reconoce límites: TypeScript no valida JSON por existir un tipo, JSON local no resuelve concurrencia multiusuario, CSP no sustituye autenticación y unas métricas agregadas no explican por sí solas la causa de una latencia.

Vuelve a [`../exercises/rubrica-final.md`](../exercises/rubrica-final.md) y puntúa tu solución por comportamiento y explicación, no por similitud con esta referencia.
