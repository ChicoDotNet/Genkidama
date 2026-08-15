# Solución de referencia — Checkpoint 01

Una dirección razonable es tratar `{30,45,60,90,120}` como contrato del **caso de uso web/aplicación**, no como invariante universal de `Appointment`.

Por ejemplo, `AppointmentService::book()` puede comprobar que `$durationMinutes` pertenece al conjunto permitido antes de generar el ID, cargar el store o guardar el candidato:

```php
if (!in_array($durationMinutes, [30, 45, 60, 90, 120], true)) {
    throw new DomainException('Selecciona una duración disponible.');
}
```

La prueba más importante usa un `AppointmentStore` falso que permita comprobar que `save()` no fue llamado para `75` minutos.

No cambies el constructor de `Appointment` a ese conjunto si deseas conservar su contrato actual de 15–480 minutos. Esa distinción deja abierta una futura API que acepte, por ejemplo, 75 minutos sin obligar al formulario actual a ofrecerlos.

En la web, `DomainException` ya conserva los valores enviados y presenta `role="alert"`; la misma ruta de error puede mostrar el mensaje nuevo.

La solución no tiene que usar exactamente estas líneas. Debe preservar la separación entre **regla universal de una cita** y **opciones admitidas por este caso de uso**.
