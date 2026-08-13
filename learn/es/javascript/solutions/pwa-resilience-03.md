# Solución de referencia — Persistencia y experiencia offline

Una solución válida mantiene `loadPreferredBoard` devolviendo `{ board, mode }`, propaga ese modo al mensaje inicial y trata por separado la disponibilidad del modo offline.

La prueba inyecta una lectura primaria que falla o no devuelve tablero y confirma que el estado previo se recupera sin mutarlo. No hace falta crear una abstracción adicional sólo para el texto.

La decisión importante es separar responsabilidades: IndexedDB/localStorage conservan datos; Cache Storage conserva la app shell; el manifest describe la aplicación para instalación y presentación.
