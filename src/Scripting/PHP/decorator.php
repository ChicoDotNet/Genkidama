<?php

declare(strict_types=1);

interface Component
{
    public function render(): string;
}

final class PlainMessage implements Component
{
    public function render(): string { return 'alert'; }
}

abstract class ComponentDecorator implements Component
{
    public function __construct(protected Component $inner) {}
}

final class AuditDecorator extends ComponentDecorator
{
    public function render(): string { return 'audit(' . $this->inner->render() . ')'; }
}

final class EncryptDecorator extends ComponentDecorator
{
    public function render(): string { return 'enc(' . $this->inner->render() . ')'; }
}

$base = new PlainMessage();
echo 'base=' . $base->render() . PHP_EOL;
echo 'audit=' . (new AuditDecorator($base))->render() . PHP_EOL;
echo 'encrypted=' . (new EncryptDecorator($base))->render() . PHP_EOL;
echo 'stacked=' . (new AuditDecorator(new EncryptDecorator($base)))->render() . PHP_EOL;
