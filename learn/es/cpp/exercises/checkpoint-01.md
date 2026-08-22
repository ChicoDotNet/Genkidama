# Checkpoint 01 — Índice confiable

## Objetivo

Modificar ThreadSeek escribiendo C++ y pruebas, sin una receta paso a paso.

## Encargo

Implementa un filtro opcional por extensión. La CLI debe aceptar:

```text
threadseek <directorio> [texto] [extension]
```

Si se proporciona `extension`, sólo deben aparecer coincidencias cuya extensión sea exactamente la solicitada ignorando mayúsculas ASCII. Acepta tanto `txt` como `.txt`.

## Restricciones

- No vuelvas a recorrer el filesystem para aplicar el filtro.
- Mantén `main.cpp` como adaptador de consola; la regla debe vivir en una superficie testeable.
- No agregues `new`, `delete` ni estado global mutable.
- Conserva orden determinista.
- Una extensión vacía equivale a no filtrar.

## Pruebas mínimas

Escribe al menos tres casos: `txt` encuentra ambos `.TXT/.txt`; `md` encuentra sólo README; una extensión inexistente devuelve cero resultados.

## Comprobación

```bash
cmake --build app/build --parallel
ctest --test-dir app/build --output-on-failure
```

Después ejecuta la CLI sobre un directorio real pequeño.

## Reflexión

¿Qué dato pertenece al índice y qué dato pertenece a la consulta? ¿Qué cambiaría si mañana quisiéramos filtrar además por tamaño?

Cuando termines, compara tu solución con `../solutions/checkpoint-01.md`.
