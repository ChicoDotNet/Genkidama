using System;

public sealed class AuthService
{
    public string Authenticate(string user) => $"auth({user})";
}

public sealed class InventoryService
{
    public string Reserve(string sku) => $"reserve({sku})";
}

public sealed class BillingService
{
    public string Charge(int cents) => $"charge({cents})";
}

public sealed class CheckoutFacade(
    AuthService auth,
    InventoryService inventory,
    BillingService billing)
{
    public string Checkout(string user, string sku, int cents)
    {
        return string.Join(">",
            auth.Authenticate(user),
            inventory.Reserve(sku),
            billing.Charge(cents));
    }
}

public static class FacadeExample
{
    public static void Main()
    {
        var facade = new CheckoutFacade(
            new AuthService(),
            new InventoryService(),
            new BillingService());

        Console.WriteLine($"checkout={facade.Checkout("alice", "SKU-42", 499)}");
    }
}
