<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('strategy');
$checkout = static fn(float $amount, callable $discount): float => $amount - $discount($amount);
$vip = static fn(float $amount): float => $amount * 0.2;
$check($checkout(100.0, $vip) === 80.0);
