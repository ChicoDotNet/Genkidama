# Solución de referencia — Checkpoint 02

Una solución razonable mantiene `reschedule` en `AppointmentService` porque orquesta entrada, dominio y persistencia; la regla temporal sigue en `Appointment`/`Schedule`.

La secuencia puede ser:

```php
$current = $schedule->find($id);
$replacement = new Appointment(
    $current->id,
    $current->clientName,
    $current->serviceName,
    $newStart,
    $durationMinutes,
);
$candidate = $schedule->replacing($replacement);
$store->save($candidate);
```

La prueba importante no es sólo que el caso válido cambie horario. Fuerza un cruce con otra cita, captura el calendario antes del intento y comprueba que después del `DomainException` el store contiene exactamente el mismo estado.

No es necesario cambiar a SQLite para resolver esta operación en el volumen actual. `AppointmentStore` ya ofrece el contrato que necesita el caso de uso: cargar estado y publicar un candidato completo. SQLite empezaría a pagar su complejidad cuando necesitemos consultas selectivas importantes, múltiples escritores, transacciones con varias tablas o un volumen donde reescribir todo el documento deje de ser razonable.

Vuelve a [`../lessons/08-ciclo-de-vida-y-checkpoint-02.md`](../lessons/08-ciclo-de-vida-y-checkpoint-02.md).
