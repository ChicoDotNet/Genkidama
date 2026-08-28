<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('mvp');
$model = static fn(): array => ['name'=>'Ada']; $rendered='';
$view = static function(string $text) use (&$rendered): void { $rendered=$text; };
$presenter = static fn() => $view('Hello '.$model()['name']);
$presenter(); $check($rendered === 'Hello Ada');
