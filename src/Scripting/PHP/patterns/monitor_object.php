<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('monitor_object');
$lock = fopen('php://temp','r+'); $counter = 0;
$critical = static function(callable $work) use ($lock): void { flock($lock, LOCK_EX); try { $work(); } finally { flock($lock, LOCK_UN); } };
$critical(static function() use (&$counter){$counter++;});
$check($counter === 1);
