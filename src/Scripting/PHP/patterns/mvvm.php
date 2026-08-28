<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('mvvm');
$model = ['first'=>'Ada','last'=>'Lovelace'];
$viewModel = new class($model) {
    public function __construct(private array $model) {}
    public function displayName(): string { return $this->model['first'].' '.$this->model['last']; }
};
$view = static fn($vm): string => $vm->displayName();
$check($view($viewModel) === 'Ada Lovelace');
