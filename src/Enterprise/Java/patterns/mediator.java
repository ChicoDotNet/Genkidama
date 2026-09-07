import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class PatternCell {
    @FunctionalInterface
    interface Receiver {
        void receive(String sender, String message);
    }

    static final class CheckoutMediator {
        private final Map<String, Receiver> colleagues = new HashMap<>();

        void register(String name, Receiver receiver) {
            colleagues.put(name, receiver);
        }

        void send(String sender, String recipient, String message) {
            Receiver receiver = colleagues.get(recipient);
            if (receiver == null) {
                throw new IllegalArgumentException("unknown colleague: " + recipient);
            }
            receiver.receive(sender, message);
        }
    }

    static boolean run() {
        CheckoutMediator mediator = new CheckoutMediator();
        List<String> events = new ArrayList<>();

        mediator.register("payment", (sender, message) ->
            events.add("payment<-" + sender + ":" + message));
        mediator.register("inventory", (sender, message) ->
            events.add("inventory<-" + sender + ":" + message));

        mediator.send("payment", "inventory", "reserve-order-42");
        mediator.send("inventory", "payment", "reserved-order-42");

        boolean unknownRejected = false;
        try {
            mediator.send("payment", "shipping", "dispatch-order-42");
        } catch (IllegalArgumentException expected) {
            unknownRejected = expected.getMessage().contains("shipping");
        }

        return events.equals(List.of(
            "inventory<-payment:reserve-order-42",
            "payment<-inventory:reserved-order-42")) && unknownRejected;
    }

    public static void main(String[] args) {
        if (!run()) {
            throw new AssertionError("Mediator contract failed");
        }
    }
}
