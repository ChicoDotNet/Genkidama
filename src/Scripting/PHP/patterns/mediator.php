<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('mediator');
$events = [];
$mediator = static function(string $sender, string $event) use (&$events): void { $events[] = "$sender:$event"; };
$componentA = static fn() => $mediator('A','ready');
$componentB = static fn() => $mediator('B','done');
$componentA(); $componentB();
$check($events === ['A:ready','B:done']);
