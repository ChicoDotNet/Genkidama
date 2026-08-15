# Lección 01 — Tu primera agenda web con PHP

## Qué vas a conseguir

Vas a ejecutar una aplicación PHP real en tu navegador y entender qué ocurre entre una URL, el servidor local y el HTML que recibes.

## Antes de empezar

Instala PHP 8.5 y Composer. Desde la carpeta `app/` del curso:

```bash
composer install
composer serve
```

Abre `http://127.0.0.1:8080`.

## El problema

Una agenda útil necesita una interfaz que cualquier persona pueda abrir sin conocer comandos. Antes de modelar citas necesitamos comprender la frontera más visible: **una petición HTTP entra y PHP produce una respuesta**.

## Concepto

PHP puede ejecutarse como CLI o detrás de un servidor web. En este curso usamos el servidor integrado sólo para desarrollo local. `public/index.php` es nuestro punto de entrada web.

La primera línea importante del código mantenido es:

```php
declare(strict_types=1);
```

No convierte PHP en un lenguaje estáticamente tipado, pero sí evita varias conversiones escalares implícitas dentro de llamadas a funciones y métodos.

## Demostración

[DEMO] Arranca `composer serve`, abre la página, cambia el ancho de la ventana y recorre con Tab los campos y el botón.

Observa que no necesitamos JavaScript para completar la tarea primaria. HTML nativo ya aporta controles de fecha, selección, labels, teclado y envío de formulario.

## Código real

Abre [`../app/public/index.php`](../app/public/index.php). Identifica configuración de zona horaria, creación del servicio, lectura de `$_SERVER`/`$_POST`/`$_GET`, salida escapada con `htmlspecialchars` y estructura semántica `main`, `header`, `section`, `form` y `table`.

La aplicación todavía no es “un archivo que hace todo”: las reglas viven en `src/` y la web sólo coordina entrada/salida.

## Qué acaba de pasar

Tu navegador pidió `/`. El servidor integrado ejecutó PHP. `index.php` construyó HTML y el navegador lo representó. Esa frontera request/response seguirá existiendo aunque después uses un framework.

## Errores comunes

- Abrir `index.php` como archivo local en vez de usar el servidor PHP.
- Mezclar reglas del negocio directamente en HTML porque “es sólo una página”.
- Imprimir entrada del usuario sin escapar.
- Creer que un control HTML `required` sustituye validación del servidor.

## Buenas prácticas

Mantén `public/` como frontera web. Usa HTML nativo antes de recrear controles con JavaScript/ARIA. La tarea principal debe ser evidente: registrar una cita.

## Tu turno

[PAUSA PARA EJERCICIO] Cambia el texto descriptivo del encabezado para explicar con tus palabras qué protege AgendaPHP. No cambies aún reglas ni persistencia.

## Cómo comprobar

```bash
php -l public/index.php
composer serve
```

Confirma por teclado que puedes llegar a cada control y que el foco es visible.

## Solución enlazada

No hay una única frase correcta. Conserva el propósito: registrar citas y evitar cruces.

## Reto adicional

Busca en el manual oficial qué diferencia existe entre el servidor integrado y una configuración de producción. No intentes convertir este curso en una guía de Nginx/Apache.

## Resumen

- PHP puede responder directamente a HTTP.
- El navegador no necesita conocer nuestras clases internas.
- HTML semántico resuelve gran parte de la interacción básica.
- Validación y reglas todavía pertenecen al servidor.

## Siguiente paso

Continúa con [Lección 02 — Tipos, clases y una cita válida](02-tipos-clases-y-citas-validas.md).

## Referencias

- [Built-in web server — PHP manual](https://www.php.net/manual/en/features.commandline.webserver.php)
- [Type declarations — PHP manual](https://www.php.net/manual/en/language.types.declarations.php)
- [HTML forms — MDN](https://developer.mozilla.org/docs/Learn_web_development/Extensions/Forms)
