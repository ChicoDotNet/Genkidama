<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('unit_of_work');
$uow = new class {
    private array $pending=[]; public array $committed=[];
    public function register(array $change): void { $this->pending[]=$change; }
    public function commit(): void { $this->committed=$this->pending; $this->pending=[]; }
};
$uow->register(['id'=>1]); $uow->commit();
$check($uow->committed === [['id'=>1]]);
