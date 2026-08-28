<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('enterprise_bridge');
$send = static fn(string $payload): string => 'http:'.$payload;
$report = static fn(string $name, callable $transport): string => $transport('report='.$name);
$check($report('sales',$send) === 'http:report=sales');
