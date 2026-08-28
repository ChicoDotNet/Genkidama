<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('microservices');
$inventoryService = static fn(string $sku): array => ['sku'=>$sku,'stock'=>4];
$orderService = static fn(string $sku, callable $inventory): bool => $inventory($sku)['stock'] > 0;
$check($orderService('A-1', $inventoryService));
