<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('state');
$states = [
    'locked' => static fn(string $event): string => $event === 'coin' ? 'unlocked' : 'locked',
    'unlocked' => static fn(string $event): string => $event === 'push' ? 'locked' : 'unlocked',
];
$state = 'locked'; $state = $states[$state]('coin'); $state = $states[$state]('push');
$check($state === 'locked');
