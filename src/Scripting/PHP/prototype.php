<?php

declare(strict_types=1);

interface Prototype
{
    public function copy(): static;
}

final class ServiceProfile implements Prototype
{
    /** @param list<string> $features */
    public function __construct(
        public string $name,
        public array $features,
    ) {
    }

    public function copy(): static
    {
        return clone $this;
    }

    public function describe(): string
    {
        return $this->name . ': ' . implode(',', $this->features);
    }
}

$original = new ServiceProfile('orders', ['metrics']);
$canary = $original->copy();
$canary->name = 'orders-canary';
$canary->features[] = 'tracing';

echo 'original=' . $original->describe() . PHP_EOL;
echo 'clone=' . $canary->describe() . PHP_EOL;
