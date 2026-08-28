<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('peer_to_peer');
$inboxes = ['a'=>[], 'b'=>[]];
$send = static function(string $from, string $to, string $msg) use (&$inboxes): void { $inboxes[$to][] = "$from:$msg"; };
$send('a','b','hello'); $send('b','a','hi');
$check($inboxes['a'] === ['b:hi'] && $inboxes['b'] === ['a:hello']);
