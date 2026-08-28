<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('pac');
$makeAgent = static function(string $name): array {
    $model = ['value'=>0];
    $control = static function(int $v) use (&$model): void { $model['value']=$v; };
    $present = static function() use ($name, &$model): string { return $name.':'.$model['value']; };
    return [$control,$present];
};
[$control,$present] = $makeAgent('counter'); $control(3);
$check($present() === 'counter:3');
