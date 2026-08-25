# Solución de referencia — Checkpoint 01

`QuoteLine` ya expresa el contrato correcto: rechaza `unitPrice < 0D`, no `<= 0D`, y usa `String.IsNullOrWhiteSpace` para la descripción.

La mejor solución del checkpoint es **proteger esas decisiones con pruebas**, no reescribir producción sin necesidad.

Agrega un test que construya una partida gratuita y espere `LineTotal = 0D`; agrega otro que espere `ArgumentException` con una descripción de espacios. En el presenter usa `UnitPriceInputValue = "0"` y comprueba que `Render` recibe una cotización válida.

La lección importante es que una regla existente también necesita evidencia: no todo ejercicio requiere añadir más código de producción.