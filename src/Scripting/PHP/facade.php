<?php

declare(strict_types=1);

final class AuthService
{
    public function authenticate(string $user): string { return "auth($user)"; }
}

final class InventoryService
{
    public function reserve(string $sku): string { return "reserve($sku)"; }
}

final class BillingService
{
    public function charge(int $cents): string { return "charge($cents)"; }
}

final class CheckoutFacade
{
    public function __construct(
        private AuthService $auth,
        private InventoryService $inventory,
        private BillingService $billing,
    ) {}

    public function checkout(string $user, string $sku, int $cents): string
    {
        return implode('>', [
            $this->auth->authenticate($user),
            $this->inventory->reserve($sku),
            $this->billing->charge($cents),
        ]);
    }
}

$facade = new CheckoutFacade(new AuthService(), new InventoryService(), new BillingService());
echo 'checkout=' . $facade->checkout('alice', 'SKU-42', 499) . PHP_EOL;
