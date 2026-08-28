<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('distributed_proxy');
$remote = static fn(string $json): string => json_encode(['result'=>json_decode($json,true)['x'] * 2]);
$proxy = static function(int $x) use ($remote): int { return json_decode($remote(json_encode(['x'=>$x])), true)['result']; };
$check($proxy(21) === 42);
