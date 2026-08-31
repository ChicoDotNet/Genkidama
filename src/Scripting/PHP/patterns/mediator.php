<?php declare(strict_types=1);

final class CheckoutMediator
{
    /** @var array<string, Closure(string, string): void> */
    private array $colleagues = [];

    public function register(string $name, Closure $receive): void
    {
        $this->colleagues[$name] = $receive;
    }

    public function send(string $sender, string $recipient, string $message): void
    {
        if (!isset($this->colleagues[$recipient])) {
            throw new RuntimeException("unknown colleague: $recipient");
        }

        ($this->colleagues[$recipient])($sender, $message);
    }
}

$events = [];
$mediator = new CheckoutMediator();
$mediator->register('inventory', static function (string $sender, string $message) use (&$events): void {
    $events[] = "inventory<-$sender:$message";
});
$mediator->register('payment', static function (string $sender, string $message) use (&$events): void {
    $events[] = "payment<-$sender:$message";
});

$payment = static fn(string $message) => $mediator->send('payment', 'inventory', $message);
$inventory = static fn(string $message) => $mediator->send('inventory', 'payment', $message);

$payment('paid');
$inventory('reserved');

if ($events !== ['inventory<-payment:paid', 'payment<-inventory:reserved']) {
    throw new RuntimeException('Mediator did not coordinate colleagues.');
}

try {
    $mediator->send('payment', 'unknown', 'ignored');
    throw new RuntimeException('Unknown colleague unexpectedly accepted.');
} catch (RuntimeException $error) {
    if (!str_starts_with($error->getMessage(), 'unknown colleague:')) {
        throw $error;
    }
}
