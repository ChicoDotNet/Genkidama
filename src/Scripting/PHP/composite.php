<?php

declare(strict_types=1);

interface Component
{
    public function size(): int;
}

final class FileLeaf implements Component
{
    public function __construct(private int $bytes) {}

    public function size(): int
    {
        return $this->bytes;
    }
}

final class FolderComposite implements Component
{
    /** @param Component[] $children */
    public function __construct(private array $children) {}

    public function size(): int
    {
        return array_sum(array_map(static fn (Component $child): int => $child->size(), $this->children));
    }
}

$readme = new FileLeaf(2);
$docs = new FolderComposite([new FileLeaf(3), new FileLeaf(5)]);
$root = new FolderComposite([$readme, $docs]);

echo 'leaf=' . $readme->size() . PHP_EOL;
echo 'docs=' . $docs->size() . PHP_EOL;
echo 'root=' . $root->size() . PHP_EOL;
