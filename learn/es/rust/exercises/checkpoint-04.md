# Checkpoint 04 — El backup contiene algo que nadie declaró

Trabaja sobre BackupForge sin abrir la solución.

## Escenario

Un backup pasa `verify`, pero un proceso externo dejó `diagnostics/raw.dump` dentro del directorio. Ese archivo no aparece en `manifest.json`.

Tu objetivo es endurecer la operación sin destruir evidencia ni redefinir silenciosamente `verify`.

## Requisitos

1. Conserva `verify` comprobando únicamente las entradas declaradas.
2. Añade o extiende una operación de auditoría que detecte entradas no declaradas recursivamente.
3. `manifest.json` no debe contarse como extra.
4. No sigas symlinks durante la inspección; una entrada no regular no debe convertirse en contenido implícitamente confiable.
5. La salida debe distinguir checksums/missing de contenido inesperado.
6. Añade al menos una regresión offline.
7. No borres automáticamente ningún hallazgo.

## Evidencia esperada

Un backup exacto debe auditar limpio. Después de crear `diagnostics/raw.dump`, la auditoría debe fallar y nombrar esa ruta relativa.

## Reflexión

Explica por qué una auditoría estricta es distinta de:
- comprobar checksums;
- un antivirus;
- permisos de filesystem;
- autenticidad criptográfica del manifest.

Cuando termines, compara con [`../solutions/checkpoint-04.md`](../solutions/checkpoint-04.md).
