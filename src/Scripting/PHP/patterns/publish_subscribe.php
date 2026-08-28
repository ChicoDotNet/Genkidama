<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('publish_subscribe');
$topics = [];
$subscribe = static function(string $topic, callable $h) use (&$topics): void { $topics[$topic][] = $h; };
$publish = static function(string $topic, mixed $m) use (&$topics): void { foreach ($topics[$topic] ?? [] as $h) $h($m); };
$seen=[]; $subscribe('news', static function($m) use (&$seen){$seen[]=$m;}); $publish('news','v1');
$check($seen === ['v1']);
