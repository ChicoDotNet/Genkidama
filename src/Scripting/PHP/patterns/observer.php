<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('observer');
$observers = [];
$subscribe = static function(callable $o) use (&$observers): void { $observers[] = $o; };
$publish = static function(string $e) use (&$observers): void { foreach ($observers as $o) $o($e); };
$seen = []; $subscribe(static function($e) use (&$seen) { $seen[] = $e; }); $publish('changed');
$check($seen === ['changed']);
