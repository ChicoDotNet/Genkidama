# Evaluación final — UptimeLab

Resuelve este encargo sin una receta de archivos o funciones. Puedes consultar documentación oficial y las lecciones, pero no abras la solución hasta terminar un intento serio.

## Historia A — Estado del target

El equipo necesita poder **deshabilitar temporalmente un target** sin borrarlo de configuración. Un target deshabilitado no debe ejecutar requests HTTP ni contribuir a summary/trends mientras permanezca deshabilitado. La representación debe ser explícita, compatible con la configuración existente y fácil de explicar.

Escribe primero pruebas que protejan el comportamiento elegido.

## Historia B — Bug de integridad

Actualmente debes asumir que una configuración puede contener targets duplicados por nombre después de normalización razonable (espacios alrededor y comparación que no dependa de mayúsculas/minúsculas). Corrige el problema para que una configuración ambigua sea rechazada antes de iniciar checks.

Añade una regresión que hubiera fallado antes del arreglo.

## Historia C — Fallas y consistencia

Conserva los contratos existentes:

- un error de transporte no es lo mismo que HTTP 500;
- una falla de persistencia no deja estado visible nuevo;
- las rutas inválidas no mutan historial;
- una cancelación debe propagarse como error/cancelación idiomática, no convertirse silenciosamente en éxito.

Demuestra al menos uno de estos contratos con una prueba nueva relacionada con tu cambio.

## Historia D — Concurrencia

Tu implementación no debe introducir goroutines sin ownership ni límites. Explica en un comentario de diseño o en tu defensa final por qué la solución conserva el modelo de concurrencia de UptimeLab y qué ocurriría con miles de targets.

## Historia E — Documentación oficial

Consulta al menos dos fuentes oficiales de Go y deja una nota breve con la decisión que sustentan. Una debe relacionarse con `context`, concurrencia, HTTP o testing; la otra puede cubrir errores, race detector o tooling.

## Historia F — Diseño de mejora

Sin implementarlo, diseña el siguiente paso para ejecutar varias instancias de UptimeLab contra un almacenamiento compartido. Identifica:

- qué interfaz/frontera sustituirías primero;
- cómo evitarías escrituras perdidas;
- qué parte del dominio debería permanecer independiente del almacenamiento;
- qué observabilidad adicional necesitarías;
- qué riesgo de seguridad/privacidad aparecería.

## Evidencia mínima

Entrega:

```bash
gofmt -w .
go vet ./...
go test -race ./...
go build ./cmd/uptimelab
```

Además muestra:

1. prueba de target deshabilitado;
2. prueba de duplicado normalizado;
3. una regresión de falla/cancelación/consistencia;
4. una ejecución manual o smoke del servidor;
5. dos referencias oficiales consultadas;
6. una defensa de arquitectura de aproximadamente cinco minutos.

Autoevalúate con [`rubrica-final.md`](rubrica-final.md).