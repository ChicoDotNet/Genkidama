<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('broker');
$services = ['echo' => static fn(array $m): array => ['reply'=>$m['text']]];
$broker = static fn(string $service, array $message): array => $services[$service]($message);
$check($broker('echo',['text'=>'hi']) === ['reply'=>'hi']);
