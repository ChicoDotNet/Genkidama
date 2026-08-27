<?php

declare(strict_types=1);

interface DocumentStore
{
    public function get(int $id): string;
}

final class RemoteDocumentStore implements DocumentStore
{
    public int $fetchCount = 0;

    public function get(int $id): string
    {
        $this->fetchCount++;
        return "doc($id)";
    }
}

final class DocumentStoreProxy implements DocumentStore
{
    private ?RemoteDocumentStore $backend = null;
    /** @var array<int, string> */
    private array $cache = [];

    public function get(int $id): string
    {
        if (array_key_exists($id, $this->cache)) {
            return $this->cache[$id];
        }
        $this->backend ??= new RemoteDocumentStore();
        $value = $this->backend->get($id);
        $this->cache[$id] = $value;
        return $value;
    }

    public function backendCount(): int
    {
        return $this->backend === null ? 0 : 1;
    }

    public function fetchCount(): int
    {
        return $this->backend?->fetchCount ?? 0;
    }
}

$store = new DocumentStoreProxy();
$first = $store->get(42);
$second = $store->get(42);
printf("backend=%d;fetches=%d;first=%s;second=%s\n", $store->backendCount(), $store->fetchCount(), $first, $second);
