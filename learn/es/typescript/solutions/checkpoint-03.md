# Solución de referencia — Checkpoint 03

La referencia reutiliza la misma frontera de persistencia; no añade una excepción especial sólo para cotizaciones.

Una prueba razonable crea un `CaptureStore` con una bandera `failNextSave`. Después:

1. crea cliente y cotización por HTTP;
2. marca `failNextSave = true`;
3. intenta cambiar la cotización a `sent`;
4. exige status `503`;
5. consulta `GET /api/quotes?status=draft` y comprueba que la cotización sigue ahí;
6. repite el `PATCH` con el store funcionando y comprueba que ahora aparece bajo `status=sent`.

La propiedad importante no es el nombre del helper, sino el orden:

```text
estado actual
   ↓
construir snapshot candidato
   ↓
await store.save(candidato)
   ↓ éxito
reemplazar memoria
```

Si `save` falla, el último paso no ocurre.

Esto no convierte JSON en una base de datos transaccional. Dos procesos todavía podrían partir del mismo snapshot y sobrescribirse mutuamente. La garantía es más pequeña y honesta: dentro de un único proceso, una mutación fallida no se anuncia en memoria como si hubiera sido durable.

Vuelve al checkpoint y compara comportamiento, no nombres de funciones ni líneas exactas.
