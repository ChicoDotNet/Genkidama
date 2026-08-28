<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('object_pool');
$pool = new SplQueue(); $pool->enqueue((object)['id'=>1]);
$acquire = static fn(): object => $pool->dequeue();
$release = static fn(object $o) => $pool->enqueue($o);
$resource=$acquire(); $release($resource); $again=$acquire();
$check($resource === $again);
