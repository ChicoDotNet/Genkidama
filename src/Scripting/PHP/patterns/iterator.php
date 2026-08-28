<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('iterator');
$collection = new class(['a','b','c']) implements IteratorAggregate {
    public function __construct(private array $items) {}
    public function getIterator(): Traversable { yield from $this->items; }
};
$check(iterator_to_array($collection->getIterator()) === ['a','b','c']);
