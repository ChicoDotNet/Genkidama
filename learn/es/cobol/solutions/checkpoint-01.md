# Solución de referencia — Checkpoint 01

La regla de horas extra debe vivir cerca del cálculo de negocio, no en parsing ni reporting.

Una solución razonable separa tres valores: horas normales, horas extra y tarifa de horas extra. Si `WS-HOURS` supera 40, conserva 40 como normales y calcula el excedente como horas extra; si no, todas son normales. La tarifa extra es 1.5 veces la tarifa normal.

El bruto se calcula sumando:

```text
horas normales × tarifa normal
+
horas extra × tarifa extra
```

Después reutiliza la deducción porcentual y el cálculo de neto existentes.

Para 45 horas, tarifa `100.00` y deducción `10.00`, la referencia espera bruto `4750.00`, deducción `475.00` y neto `4275.00`.

Agrega una prueba para ese caso y conserva otra con 40 horas para demostrar que la nueva rama no cambia el comportamiento anterior.

## Criterio de diseño

La regla pertenece al cálculo porque transforma datos ya validados en importes de negocio. `PROCESS-RECORD` debe seguir coordinando parsing y validación; `WRITE-PAYROLL-LINE` debe seguir limitándose a presentar resultados.
