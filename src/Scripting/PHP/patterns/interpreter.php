<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('interpreter');
$literal = static fn(int $n) => static fn(array $ctx): int => $n;
$variable = static fn(string $name) => static fn(array $ctx): int => $ctx[$name];
$add = static fn(callable $a, callable $b) => static fn(array $ctx): int => $a($ctx) + $b($ctx);
$expr = $add($variable('x'), $literal(2));
$check($expr(['x' => 5]) === 7);
