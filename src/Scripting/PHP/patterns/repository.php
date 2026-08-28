<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('repository');
$repo = new class {
    private array $items=[];
    public function add(int $id, object $entity): void { $this->items[$id]=$entity; }
    public function get(int $id): ?object { return $this->items[$id] ?? null; }
};
$entity=(object)['name'=>'Ada']; $repo->add(1,$entity);
$check($repo->get(1) === $entity);
