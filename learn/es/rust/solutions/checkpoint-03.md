# Solución de referencia — Checkpoint 03

> Lee esto sólo después de intentar el checkpoint.

Una solución razonable construye primero `snapshots/.<nombre>.partial`, genera allí el backup y verifica su manifest. Sólo entonces `fs::rename` publica el directorio con el nombre final.

Antes de empezar se valida el nombre y se rechaza si `snapshots/<nombre>` ya existe. También se rechaza un `.partial` preexistente: borrarlo silenciosamente podría destruir evidencia de una ejecución interrumpida.

`list_snapshots` ignora nombres que comienzan con `.` porque no representan versiones publicadas. Esa regla evita confundir estado intermedio con historial, pero no pretende ser recuperación automática.

Pruebas mínimas útiles:
- crear un snapshot y listarlo;
- rechazar `../fuera`;
- rechazar reutilizar un nombre;
- ignorar un `.partial`;
- corromper bytes y demostrar que restore no escribe.

La idea central es **publicación atómica de identidad**, no inventar un framework de almacenamiento. Si en el futuro BackupForge necesita varios procesos, remote object storage o retención automática, esa nueva necesidad sí justificaría revisar la frontera.
