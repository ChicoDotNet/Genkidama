<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('visitor');
$nodes = [['type'=>'text','value'=>'Hi'], ['type'=>'number','value'=>3]];
$visitor = [
    'text' => static fn(array $n): string => $n['value'],
    'number' => static fn(array $n): string => (string)($n['value'] * 2),
];
$out = array_map(static fn($n) => $visitor[$n['type']]($n), $nodes);
$check($out === ['Hi','6']);
