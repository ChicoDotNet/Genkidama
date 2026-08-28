<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('microkernel');
$plugins = [];
$register = static function(string $name, callable $plugin) use (&$plugins): void { $plugins[$name] = $plugin; };
$run = static fn(string $name, mixed $input) => $plugins[$name]($input);
$register('upper', 'strtoupper');
$check($run('upper','kernel') === 'KERNEL');
