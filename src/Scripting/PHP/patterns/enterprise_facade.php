<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('enterprise_facade');
$reserve = static fn(string $sku): bool => $sku === 'A';
$charge = static fn(int $amount): bool => $amount > 0;
$facade = static fn(string $sku, int $amount): bool => $reserve($sku) && $charge($amount);
$check($facade('A',100));
