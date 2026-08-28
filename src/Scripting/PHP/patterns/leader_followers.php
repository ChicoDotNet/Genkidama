<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('leader_followers');
$events = new SplQueue(); foreach (['a','b','c'] as $e) $events->enqueue($e);
$workers = ['w1','w2']; $handled = [];
$i = 0; while (!$events->isEmpty()) { $leader = $workers[$i++ % 2]; $handled[] = $leader.':'.$events->dequeue(); }
$check($handled === ['w1:a','w2:b','w1:c']);
