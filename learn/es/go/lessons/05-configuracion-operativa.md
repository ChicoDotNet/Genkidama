# Lección 05 — Configuración operativa sin contaminar el dominio

## Qué vas a conseguir

Convertirás la configuración de UptimeLab en un contrato explícito: targets, archivo de historial e intervalo de ejecución se resuelven en `cmd/uptimelab`, mientras `monitor` permanece independiente del entorno del proceso.

## Antes de empezar

Completa la [Lección 04](04-api-dashboard-y-checkpoint.md) y confirma que `go test -race ./...` está verde.

## El problema

Hasta ahora UptimeLab ejecuta checks bajo demanda. Para operar durante horas necesitamos decidir dónde guardar historial y con qué frecuencia ejecutar el lote. Leer variables de entorno desde cualquier paquete sería rápido, pero volvería las reglas difíciles de probar y reutilizar.

## Concepto

El ejecutable es una **frontera de composición**. Ahí traducimos texto externo a tipos Go:

- `UPTIMELAB_TARGETS` → `[]monitor.Target`;
- `UPTIMELAB_HISTORY_FILE` → ruta local;
- `UPTIMELAB_INTERVAL` → `time.Duration`.

Una vez convertidos, los paquetes internos reciben valores ya tipados. `monitor.Checker` no sabe que existe `os.Getenv`.

## Demostración

[EN PANTALLA] Abre `cmd/uptimelab/main.go` y localiza `parseInterval`.

Acepta segundos sencillos (`30`) o duraciones idiomáticas (`2m`, `500ms`), y rechaza cero negativo o texto inválido.

[EJECUTAR]

```bash
cd app
go test ./cmd/uptimelab
UPTIMELAB_INTERVAL=30 go run ./cmd/uptimelab
```

En PowerShell:

```powershell
$env:UPTIMELAB_INTERVAL='30'
go run ./cmd/uptimelab
```

## Código real

El proceso usa `signal.NotifyContext` para obtener un contexto cancelable por `Ctrl+C`/SIGTERM. Esa decisión permite que HTTP y scheduling compartan la misma señal de apagado sin variables globales.

## Qué acaba de pasar

Configuración externa dejó de ser “texto mágico” disperso. Se valida una vez en el borde y luego circula como tipos normales.

## Errores comunes

- Leer variables de entorno desde `monitor` o `history`.
- Interpretar `0` como un intervalo válido y crear un loop ocupado.
- Hacer `panic` por una entrada de usuario cuando un error descriptivo permite diagnosticar mejor.
- Mezclar configuración con reglas de salud HTTP.

## Buenas prácticas

Valida temprano, usa `time.ParseDuration` cuando el ecosistema ya define una sintaxis estándar y conserva defaults pequeños/documentados.

## Tu turno

[PAUSA PARA EJERCICIO] Añade un caso de prueba para un intervalo de `1500ms` y explica por qué `time.Duration` es mejor contrato interno que un entero ambiguo.

## Cómo comprobar

```bash
gofmt -w .
go vet ./...
go test -race ./...
```

## Solución enlazada

No hay una solución separada para este microejercicio: compara tu cambio con el comportamiento de `parseInterval` y conserva la suite verde.

## Reto adicional

Diseña cómo incorporarías un límite de concurrencia configurable sin permitir valores cero o absurdamente grandes. No lo implementes todavía.

## Resumen

El proceso traduce configuración externa a contratos tipados; los paquetes de negocio siguen ignorando de dónde vinieron los valores.

## Siguiente paso

Continúa con la [Lección 06 — Historial persistente](06-historial-persistente.md): persistiremos resultados reales y trataremos un archivo corrupto como error explícito, no como “historial vacío”.

## Referencias

- https://pkg.go.dev/os
- https://pkg.go.dev/time#ParseDuration
- https://pkg.go.dev/os/signal#NotifyContext
