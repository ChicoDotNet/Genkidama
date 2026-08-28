<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('active_object');
$queue = new SplQueue(); $result = [];
$submit = static fn(callable $job) => $queue->enqueue($job);
$submit(static function() use (&$result){$result[]='done';});
while (!$queue->isEmpty()) { ($queue->dequeue())(); }
$check($result === ['done']);
