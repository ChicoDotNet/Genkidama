<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('half_sync_half_async');
$asyncQueue = new SplQueue(); $processed = [];
$accept = static fn(string $event) => $asyncQueue->enqueue($event);
$syncWorker = static function() use ($asyncQueue, &$processed): void { while (!$asyncQueue->isEmpty()) $processed[] = strtoupper($asyncQueue->dequeue()); };
$accept('a'); $accept('b'); $syncWorker();
$check($processed === ['A','B']);
