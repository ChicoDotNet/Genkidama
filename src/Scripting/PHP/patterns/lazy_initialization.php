<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('lazy_initialization');
$created=0; $value=null;
$get = static function() use (&$created, &$value): object { if ($value === null) { $created++; $value=(object)['id'=>1]; } return $value; };
$a=$get(); $b=$get();
$check($a === $b && $created === 1);
