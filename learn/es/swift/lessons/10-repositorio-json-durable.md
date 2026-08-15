# Lección 10 — Repositorio JSON durable

## Qué vas a conseguir

Implementarás `FileTimeQuoteRepository`, una segunda implementación del mismo contrato que persiste `TimeQuoteBook` como JSON.

## El problema

Una app real necesita sobrevivir entre ejecuciones. El objetivo no es cambiar dominio ni servicio, sino sustituir infraestructura detrás de una frontera ya probada.

## Concepto

`FileTimeQuoteRepository` recibe una `URL`, devuelve un libro vacío cuando el archivo aún no existe y usa `JSONDecoder`/`JSONEncoder` para leer y escribir el estado. La escritura usa la opción `.atomic` para reducir el riesgo de dejar un archivo parcialmente escrito.

[EN PANTALLA]

```swift
public struct FileTimeQuoteRepository: TimeQuoteRepository {
    public mutating func load() throws -> TimeQuoteBook { ... }
    public mutating func save(_ book: TimeQuoteBook) throws { ... }
}
```

## Demostración

[EJECUTAR]

```bash
cd app
swift test
```

Busca la prueba `fileRepositorySurvivesRecreation`: crea un servicio, persiste datos, destruye esa instancia y crea otra apuntando al mismo archivo.

## Tu turno

Cambia únicamente la URL del archivo y comprueba que el servicio no necesita otra modificación.

## Cómo comprobar

Si `Client`, `TimeEntry` o `TimeQuoteService` empiezan a recibir rutas de archivo, la frontera quedó mal colocada.

## Errores comunes

- Guardar después de cada campo en vez de después de una operación coherente.
- Suponer que un archivo existe siempre.
- Mezclar rutas absolutas de una máquina concreta con lógica del dominio.

## Buenas prácticas

Inyecta la ubicación del archivo y mantén el protocolo pequeño. El almacenamiento puede cambiar después sin obligar al dominio a conocerlo.

## Resumen

El mismo caso de uso ya puede ejecutarse con memoria o con persistencia durable.

## Siguiente paso

Continúa con [la lección 11](11-fallos-de-io-explicitos.md).
