<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('client_server');
$server = static fn(array $request): array => ['status'=>200,'body'=>strtoupper($request['body'])];
$client = static fn(string $body): array => $server(['body'=>$body]);
$check($client('ping')['body'] === 'PING');
