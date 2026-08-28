<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('service_locator');
$services = ['clock' => static fn(): string => '12:00'];
$locate = static fn(string $name): callable => $services[$name] ?? throw new RuntimeException('missing');
$check($locate('clock')() === '12:00');
