using System;
using System.Collections.Generic;

namespace Genkidama.PatternExamples;

public static class MediatorExample
{
    public static bool Run()
    {
        var events = new List<string>();
        var colleagues = new Dictionary<string, Action<string, string>>(StringComparer.Ordinal)
        {
            ["inventory"] = (sender, message) => events.Add($"inventory<-{sender}:{message}"),
            ["payment"] = (sender, message) => events.Add($"payment<-{sender}:{message}")
        };

        void Send(string sender, string recipient, string message)
        {
            if (!colleagues.TryGetValue(recipient, out var receiver))
            {
                throw new InvalidOperationException($"Unknown colleague: {recipient}");
            }

            receiver(sender, message);
        }

        void Payment(string message) => Send("payment", "inventory", message);
        void Inventory(string message) => Send("inventory", "payment", message);

        Payment("paid");
        Inventory("reserved");

        var rejectedUnknown = false;
        try
        {
            Send("payment", "shipping", "paid");
        }
        catch (InvalidOperationException)
        {
            rejectedUnknown = true;
        }

        return string.Join('>', events) ==
               "inventory<-payment:paid>payment<-inventory:reserved" &&
               rejectedUnknown;
    }
}
