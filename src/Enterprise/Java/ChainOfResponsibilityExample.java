import java.util.ArrayList;
import java.util.List;

public final class ChainOfResponsibilityExample {
    private abstract static class RefundHandler {
        private RefundHandler next;

        RefundHandler setNext(RefundHandler nextHandler) {
            next = nextHandler;
            return nextHandler;
        }

        final String handle(int amount, List<String> visited) {
            visited.add(name());
            if (canHandle(amount)) {
                return name();
            }
            if (next == null) {
                throw new IllegalStateException("No handler accepted the request.");
            }
            return next.handle(amount, visited);
        }

        abstract String name();
        abstract boolean canHandle(int amount);
    }

    private static final class FaqHandler extends RefundHandler {
        String name() { return "faq"; }
        boolean canHandle(int amount) { return amount <= 50; }
    }

    private static final class BillingHandler extends RefundHandler {
        String name() { return "billing"; }
        boolean canHandle(int amount) { return amount <= 500; }
    }

    private static final class EscalationHandler extends RefundHandler {
        String name() { return "escalation"; }
        boolean canHandle(int amount) { return true; }
    }

    public static void main(String[] args) {
        var faq = new FaqHandler();
        var billing = new BillingHandler();
        var escalation = new EscalationHandler();
        faq.setNext(billing).setNext(escalation);

        var visited = new ArrayList<String>();
        var handled = faq.handle(250, visited);
        System.out.printf("visited=%s;handled=%s;result=refund(250)%n", String.join(">", visited), handled);
    }
}
