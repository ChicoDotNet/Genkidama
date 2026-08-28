<?php

declare(strict_types=1);

abstract class RefundHandler
{
    private ?RefundHandler $next = null;

    public function setNext(RefundHandler $next): RefundHandler
    {
        $this->next = $next;
        return $next;
    }

    public function handle(int $amount, array &$visited): string
    {
        $visited[] = $this->name();
        if ($this->canHandle($amount)) {
            return $this->name();
        }
        if ($this->next === null) {
            throw new RuntimeException('No handler accepted the request.');
        }
        return $this->next->handle($amount, $visited);
    }

    abstract protected function name(): string;
    abstract protected function canHandle(int $amount): bool;
}

final class FaqHandler extends RefundHandler
{
    protected function name(): string { return 'faq'; }
    protected function canHandle(int $amount): bool { return $amount <= 50; }
}

final class BillingHandler extends RefundHandler
{
    protected function name(): string { return 'billing'; }
    protected function canHandle(int $amount): bool { return $amount <= 500; }
}

final class EscalationHandler extends RefundHandler
{
    protected function name(): string { return 'escalation'; }
    protected function canHandle(int $amount): bool { return true; }
}

$faq = new FaqHandler();
$billing = new BillingHandler();
$escalation = new EscalationHandler();
$faq->setNext($billing)->setNext($escalation);

$visited = [];
$handled = $faq->handle(250, $visited);
echo 'visited=' . implode('>', $visited) . ';handled=' . $handled . ';result=refund(250)' . PHP_EOL;
