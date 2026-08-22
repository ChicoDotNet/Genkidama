<?php
final class ProcessRegistry
{
    private static ?ProcessRegistry $instance = null;
    private int $count = 0;

    private function __construct() {}

    public static function instance(): ProcessRegistry
    {
        return self::$instance ??= new ProcessRegistry();
    }

    public function increment(): void { $this->count++; }
    public function count(): int { return $this->count; }
}

$first = ProcessRegistry::instance();
$second = ProcessRegistry::instance();
$first->increment();
echo 'same=' . (($first === $second) ? 'true' : 'false') . PHP_EOL;
echo 'count=' . $second->count() . PHP_EOL;
