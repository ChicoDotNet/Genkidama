<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('command');
$log = [];
$command = static function () use (&$log): void { $log[] = 'saved'; };
$invoker = static fn(callable $c) => $c();
$invoker($command);
$check($log === ['saved']);
