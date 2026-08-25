# Evaluación final — QuoteRules

Trabaja sobre una copia limpia del curso y no abras la solución de referencia hasta terminar tu primer intento.

## Encargo

Extiende QuoteRules para aceptar un **código de referencia opcional** asociado a la cotización y mostrarlo en el reporte cuando exista.

El cambio debe conservar estas propiedades:

- las reglas monetarias existentes no cambian;
- una referencia vacía o inválida no llega a persistencia;
- la ausencia de referencia conserva el comportamiento actual;
- el reporte sigue siendo determinista;
- los errores continúan siendo explícitos;
- la suite existente permanece verde.

## Evidencia requerida

1. Una modificación funcional sobre el código existente.
2. Un bug controlado que hayas introducido y corregido, con explicación de causa.
3. Al menos una prueba de caso feliz y una prueba de failure mode.
4. Un comando CLI que demuestre una referencia válida.
5. Un comando o test que demuestre el rechazo de una referencia inválida.
6. Una referencia a documentación oficial consultada.
7. Una propuesta breve de siguiente mejora y el módulo donde viviría.

## Restricciones

No hay receta de archivos, nombres o funciones. Debes inferir la estructura a partir del código actual. No añadas dependencias externas salvo que puedas demostrar una necesidad que la plataforma estándar no cubre.

## Comprobación mínima

```bash
dotnet build app/QuoteRules/QuoteRules.fsproj --configuration Release
dotnet test app/QuoteRules.Tests/QuoteRules.Tests.fsproj --configuration Release
```

Después usa la [rúbrica de la lección 16](../lessons/16-rubrica-y-solucion.md).

Sólo tras completar tu intento: [solución de referencia](../solutions/evaluacion-final-referencia.md).
