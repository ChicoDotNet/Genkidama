# Lección 17 — Evaluación final: extiende StockFlow sin receta

## Qué vas a conseguir

Esta vez no hay una secuencia de edición. Vas a recibir una tarea parecida a una historia de trabajo junior: entender una base existente, agregar una capacidad, corregir un defecto, protegerla con pruebas y explicar decisiones.

## Antes de empezar

Completa los cuatro checkpoints y deja la suite verde.

Crea una copia de trabajo de StockFlow. No consultes la solución hasta haber realizado un intento completo.

## El contexto

StockFlow administra catálogo y pedidos, pero todavía no completa la tercera parte de su propósito: una **facturación simplificada para fines educativos**.

No vas a implementar CFDI, impuestos reales ni cumplimiento fiscal. La meta es modelar un comprobante interno sencillo derivado de un pedido existente y demostrar que puedes extender una aplicación sin convertir la tarea en un rediseño total.

## Tu misión

Abre [`../exercises/evaluacion-final.md`](../exercises/evaluacion-final.md). El trabajo tiene siete dimensiones:

1. leer y explicar la arquitectura actual;
2. agregar una factura simplificada derivada de un pedido;
3. corregir un bug real de entrada nula;
4. mantener errores HTTP explícitos;
5. agregar pruebas del nivel adecuado;
6. consultar documentación oficial para una decisión;
7. proponer una mejora arquitectónica sin implementarla innecesariamente.

No hay instrucciones de archivo por archivo.

## Cómo trabajar

Empieza por ejecutar:

```bash
dotnet test app/tests/StockFlow.Api.Tests/StockFlow.Api.Tests.csproj
```

Después lee el código antes de editar. Identifica qué pertenece a HTTP, qué pertenece al caso de uso y qué pertenece a persistencia.

Cuando termines, vuelve a ejecutar build y tests y levanta la API manualmente para demostrar la nueva capacidad.

## Evidencia esperada

Tu entrega debe permitir a otra persona comprobar:

- código que compila;
- tests verdes;
- al menos una prueba nueva de comportamiento;
- un endpoint utilizable para la capacidad nueva;
- un error corregido y protegido contra regresión;
- una breve nota de diseño;
- una referencia a documentación oficial consultada.

## No optimices para la rúbrica

La [`rúbrica`](../rubric.md) existe para evaluar evidencia, no para premiar cantidad de archivos ni patrones sofisticados.

Una solución pequeña, explicable y bien probada supera a una arquitectura enorme que no puedes defender.

## Después del intento

Compara tu enfoque con la [`solución de referencia`](../solutions/evaluacion-final.md). No necesitas coincidir con ella. Si tu diseño satisface criterios, mantiene claridad y puedes explicar sus trade-offs, puede ser igualmente válido.

## Cómo hablar de esta evaluación en una entrevista

No digas “construí un ERP”. Describe con precisión:

- recibí una API existente;
- identifiqué sus fronteras;
- agregué una capacidad de facturación simplificada;
- corregí un fallo de entrada;
- protegí contratos con pruebas;
- expliqué qué faltaría para producción.

Eso comunica aprendizaje real sin inflar alcance.

## Cierre

Si puedes resolver esta evaluación sin receta, leer los fallos de tus tests y defender por qué colocaste cada responsabilidad donde está, has alcanzado el objetivo del curso: una base razonable para intentar trabajo junior con supervisión y seguir creciendo sobre experiencia real.

El curso no garantiza contratación. Sí busca dejar una aplicación y una conversación técnica que demuestren mejor lo que ya sabes hacer.
