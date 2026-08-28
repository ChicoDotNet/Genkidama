<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('memento');
$originator = new class {
    public string $state = 'draft';
    public function save(): array { return ['state' => $this->state]; }
    public function restore(array $memento): void { $this->state = $memento['state']; }
};
$m = $originator->save(); $originator->state = 'published'; $originator->restore($m);
$check($originator->state === 'draft');
