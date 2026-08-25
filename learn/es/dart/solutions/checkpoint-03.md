# Solución de referencia — Checkpoint 03

Una solución simple conserva el cálculo dentro del reporte y define explícitamente el desempate.

```dart
ExpenseCategory? get largestCategory {
  if (totalsByCategory.isEmpty) {
    return null;
  }

  return totalsByCategory.entries.reduce((best, current) {
    if (current.value > best.value) {
      return current;
    }
    if (current.value == best.value &&
        current.key.index < best.key.index) {
      return current;
    }
    return best;
  }).key;
}
```

Aquí el empate se resuelve por el orden declarado del enum. Esa regla no es “la única correcta”; lo importante es que sea explícita y esté probada.

Pruebas mínimas:

- reporte vacío → `largestCategory == null`;
- dos categorías con montos distintos → gana la de mayor total;
- empate → gana la categoría con menor `index` del enum.

En la UI, consulta `monthlyReport.largestCategory` y renderiza una línea adicional sólo cuando no sea `null`. No copies los totales a otro estado del widget.

El trade-off elegido es mantener el resumen derivado y determinista. Si en el futuro el orden del enum deja de representar una prioridad útil, la regla de desempate debería convertirse en una política explícita del producto.
