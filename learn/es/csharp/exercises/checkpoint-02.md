# Checkpoint 02 — Protege un límite de pedido

## Contexto

StockFlow ya puede crear pedidos y reservar inventario de forma completa.

El negocio agrega esta regla:

> Ninguna línea individual puede solicitar más de 10 unidades.

Un pedido con una línea de 11 o más debe rechazarse **antes de modificar inventario**.

## Tu trabajo

Sin seguir una receta de archivos o líneas:

- localiza dónde debe vivir la regla;
- devuelve un error que ayude a entender la causa;
- agrega al menos una prueba automática;
- comprueba que el stock no cambia cuando el pedido es rechazado;
- conserva todas las pruebas existentes en verde.

## Criterios de aceptación

1. cantidad 10 puede continuar si existe stock suficiente;
2. cantidad 11 se rechaza;
3. el error es explícito;
4. el inventario queda intacto cuando se rechaza;
5. `dotnet test` termina en verde.

## Restricción

No resuelvas el requisito sólo en el endpoint HTTP. La regla debe seguir aplicando cuando el caso de uso se invoque directamente desde código.

Cuando hayas terminado, compara tu enfoque con [`../solutions/checkpoint-02.md`](../solutions/checkpoint-02.md).
