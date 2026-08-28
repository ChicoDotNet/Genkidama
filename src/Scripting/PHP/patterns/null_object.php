<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('null_object');
$nullLogger = new class { public function log(string $message): void {} };
$service = static function(object $logger): string { $logger->log('work'); return 'done'; };
$check($service($nullLogger) === 'done');
