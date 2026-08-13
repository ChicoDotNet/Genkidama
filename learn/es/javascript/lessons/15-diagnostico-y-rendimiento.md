# Lección 15 — Diagnóstico antes de optimizar

## Qué vas a conseguir
Instrumentarás operaciones reales sin contaminar el comportamiento normal de la app. Aprenderás a medir antes de optimizar y a diseñar diagnósticos que también puedan probarse.

## Antes de empezar
Ejecuta `npm run verify`, después inicia la app con `npm start`. Abre una vez `http://127.0.0.1:4173` y otra con `?debug=1`.

## El problema
“Se siente lento” no es un diagnóstico. Tampoco lo es llenar el código de `console.log` permanentes o intentar optimizar una función porque parece sofisticada.

En Kanban Local hay operaciones de naturaleza distinta: render síncrono, parsing de un JSON y persistencia asíncrona. Si no medimos las fronteras correctas podemos atacar el lugar equivocado.

## Concepto
`diagnostics.js` crea un instrumento opt-in:

```js
const diagnostics = createDiagnostics({
  enabled: new URLSearchParams(window.location.search).get("debug") === "1",
});
```

Cuando `debug=1`, `measure` y `measureAsync` registran duración. En uso normal ejecutan la operación sin emitir diagnóstico.

El reloj y la salida son inyectables en pruebas. Eso evita pruebas que dependan de “esperar 20 ms” y permite demostrar exactamente qué se registra.

Usamos `performance.now()` porque mide intervalos con un reloj monotónico apropiado para duración. No necesitamos convertir esa medición en una promesa de precisión que el entorno no ofrece; por eso redondeamos a centésimas de milisegundo.

## Demostración
[DEMO] Abre la app con `?debug=1`, crea una tarjeta, importa un JSON y observa entradas como:

```text
[Kanban Local] { event: "timing", label: "ui.render", durationMs: ... }
[Kanban Local] { event: "timing", label: "persistence.save", durationMs: ... }
```

[EJECUTAR]

```powershell
npm test
```

Revisa `diagnostics.test.js`: un reloj falso permite comprobar mediciones síncronas y asíncronas sin hacer la suite más lenta.

## Código real
La instrumentación rodea fronteras, no cada línea:

- `ui.render` mide pintar el tablero;
- `persistence.save` mide guardar por el repositorio preferido;
- `import.read` mide leer el archivo;
- `import.parse` mide validar y convertir JSON.

Eso produce datos que se pueden comparar con una hipótesis. Si el render tarda poco pero IndexedDB domina el tiempo, optimizar el filtro de tarjetas no resolverá el problema observado.

## Qué acaba de pasar
Añadimos observabilidad sin convertirla en dependencia global ni mezclarla con reglas de negocio. Además, la API de diagnóstico está diseñada para ser eliminable: el dominio no sabe que existe.

## Errores comunes
- medir con `Date.now()` y asumir que cualquier timestamp sirve igual para duración;
- optimizar antes de reproducir el problema;
- dejar logs sensibles o ruidosos encendidos por defecto;
- usar benchmarks diminutos para justificar cambios que empeoran legibilidad;
- hacer pruebas que dependen de tiempos reales y se vuelven flaky.

## Buenas prácticas
Formula primero una pregunta: “¿qué operación sospecho que cuesta?”. Mide la frontera, repite el escenario, compara y sólo entonces cambia código. Conserva el diagnóstico si ayuda a futuras regresiones; elimínalo si sólo añade ruido.

## Tu turno
Agrega localmente una medición alrededor de una operación que todavía no esté instrumentada. Explica qué decisión concreta tomarías si la medición fuera alta y qué harías si fuera baja.

## Cómo comprobar
Ejecuta `npm run verify`. Con `?debug=1` deben aparecer mediciones; sin ese parámetro la consola no debe llenarse con timings de Kanban Local.

## Solución
Una buena respuesta conecta **señal → hipótesis → decisión**. “Medir todo por si acaso” no es mejor que no medir nada.

## Reto adicional
Diseña un presupuesto de rendimiento para un tablero con 1,000 tarjetas. No necesitas implementarlo: define qué medirías, en qué hardware/navegador y qué umbral consideras útil para la experiencia.

## Resumen
- rendimiento se diagnostica con evidencia;
- `performance.now()` sirve para intervalos, no para fechas de negocio;
- los diagnósticos pueden ser opt-in y testeables;
- instrumentar fronteras reduce ruido;
- una optimización sin hipótesis es sólo complejidad nueva.

## Siguiente paso
Continúa con la [Lección 16 — Hardening y checkpoint 04](16-seguridad-hardening-y-checkpoint.md).

## Referencias
- [`performance.now()` — MDN](https://developer.mozilla.org/docs/Web/API/Performance/now)
- [Performance APIs — MDN](https://developer.mozilla.org/docs/Web/API/Performance)
