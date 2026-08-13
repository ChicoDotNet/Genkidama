# Lección 14 — Un gate profesional y reproducible

## Qué vas a conseguir
Convertirás varios comandos de calidad en un contrato único que puedas ejecutar igual en tu máquina y en CI: `npm run verify`.

## Antes de empezar
Ejecuta `npm run verify` desde `app/` y abre `package.json` junto con `.github/workflows/learn-javascript.yml`.

## El problema
Es fácil terminar con una lista de instrucciones que sólo conoce quien creó el proyecto:

```text
corre esto, luego aquello, no olvides el smoke, y CI hace otra cosa ligeramente diferente
```

Ese modelo genera drift. Una persona puede decir “en mi máquina está verde” mientras CI valida otra combinación.

También existe el extremo contrario: instalar linter, formatter, bundler, framework de tests y media docena de plugins antes de que exista un problema que los justifique. Cada herramienta tiene costo de actualización y supply chain.

## Concepto
El tooling profesional reduce incertidumbre. Para este curso, Node y npm ya ofrecen lo necesario para un gate inicial:

```json
{
  "scripts": {
    "check": "...",
    "test": "node --test tests/*.test.js",
    "smoke": "node tools/smoke.mjs",
    "verify": "npm run check && npm test && npm run smoke"
  }
}
```

`verify` es la puerta local. El workflow JavaScript ejecuta exactamente esa puerta y agrega una comprobación HTTP porque sólo CI levanta el servidor para verificar headers y MIME types.

`check` tampoco significa únicamente “la sintaxis compila”: también ejecuta `validate-pwa.mjs`, que comprueba manifest, assets y sincronía entre el grafo de imports y `APP_SHELL`.

## Demostración
[EJECUTAR]

```powershell
npm run verify
```

Después rompe temporalmente el nombre de un módulo importado o elimina un asset de `APP_SHELL`. Observa qué gate falla y restaura el archivo.

[DEMO] Compara el script local con el workflow. La intención es que CI componga los mismos contratos, no que tenga lógica secreta.

## Código real
El workflow quedó reducido a dos bloques conceptuales:

1. preparar Node 24.18.1 LTS y ejecutar `npm run verify`;
2. levantar `npm start` y comprobar por HTTP que la aplicación real sirve HTML, manifest, service worker y headers defensivos.

No agregamos una dependencia externa sólo para poder decir que “ya usamos tooling profesional”. Si más adelante ESLint, Prettier, Playwright o un bundler aportan valor medible, deberán entrar con una razón y su propio costo de mantenimiento.

## Qué acaba de pasar
Ya existe una respuesta corta a “¿cómo sé que este proyecto está sano?”:

```powershell
npm run verify
```

Eso mejora onboarding, CI y diagnóstico. También crea una frontera útil para futuros cambios de tooling: podemos reemplazar una implementación interna sin cambiar el contrato que usa el desarrollador.

## Errores comunes
- tener comandos distintos en README y CI;
- llamar `build` a algo que realmente no construye ningún artefacto;
- introducir dependencias para problemas que el runtime ya resuelve;
- ignorar warnings porque “las pruebas pasan”;
- ejecutar localmente sólo la prueba que acabamos de escribir y nunca el gate completo.

## Buenas prácticas
El nombre del script debe describir la intención. El gate debe ser determinista, suficientemente rápido para usarse con frecuencia y estricto sobre lo que realmente promete.

## Tu turno
Añade deliberadamente una referencia inválida en una copia local y identifica qué parte de `verify` la detecta. Después documenta en una frase qué valida cada subcomando: `check`, `test`, `smoke`.

## Cómo comprobar
Restaura los cambios experimentales y exige:

```powershell
npm run verify
```

con salida exitosa.

## Solución
No hay un único texto correcto. Lo esencial es poder señalar una evidencia distinta para sintaxis/integridad, pruebas de comportamiento y smoke de aplicación.

## Reto adicional
Propón una dependencia de tooling que **sí** agregarías ahora y otra que **no** agregarías. Para cada una escribe beneficio, costo de mantenimiento y qué fallo nuevo detectaría.

## Resumen
- CI y desarrollo local deben compartir contrato;
- `npm run verify` concentra el gate del curso;
- tooling no significa acumular paquetes;
- una dependencia debe comprar una capacidad concreta;
- el gate es parte del producto porque reduce regresiones futuras.

## Siguiente paso
Continúa con la [Lección 15 — Diagnóstico antes de optimizar](15-diagnostico-y-rendimiento.md).

## Referencias
- [npm scripts — npm Docs](https://docs.npmjs.com/cli/using-npm/scripts)
- [Test runner — Node.js](https://nodejs.org/api/test.html)
