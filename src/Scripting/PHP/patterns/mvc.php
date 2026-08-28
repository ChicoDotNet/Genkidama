<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('mvc');
$model = ['name' => 'Ada'];
$controller = static function(string $name) use (&$model): void { $model['name'] = $name; };
$view = static fn(array $m): string => '<h1>'.$m['name'].'</h1>';
$controller('Grace');
$check($view($model) === '<h1>Grace</h1>');
