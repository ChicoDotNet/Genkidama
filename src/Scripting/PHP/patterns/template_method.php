<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('template_method');
$template = static function(callable $read, callable $transform, callable $write): string {
    return $write($transform($read()));
};
$result = $template(static fn() => ' genkidama ', 'trim', static fn($v) => strtoupper($v));
$check($result === 'GENKIDAMA');
