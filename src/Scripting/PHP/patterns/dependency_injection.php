<?php declare(strict_types=1);
$check = static fn(bool $ok) => $ok ?: throw new RuntimeException('dependency_injection');
$clock = static fn(): string => '10:00';
$service = new class($clock) {
    public function __construct(private Closure $clock) {}
    public function stamp(): string { return ($this->clock)(); }
};
$check($service->stamp() === '10:00');
