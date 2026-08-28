<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('message_bus');
$handlers = [];
$on = static function(string $type, callable $h) use (&$handlers): void { $handlers[$type][] = $h; };
$send = static function(string $type, mixed $m) use (&$handlers): void { foreach ($handlers[$type] ?? [] as $h) $h($m); };
$seen = []; $on('order', static function($m) use (&$seen){$seen[]=$m;}); $send('order', 42);
$check($seen === [42]);
