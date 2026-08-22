public final class DecoratorExample {
    interface Component { String render(); }
    static final class PlainMessage implements Component { public String render() { return "alert"; } }
    abstract static class ComponentDecorator implements Component {
        protected final Component inner;
        ComponentDecorator(Component inner) { this.inner = inner; }
    }
    static final class AuditDecorator extends ComponentDecorator {
        AuditDecorator(Component inner) { super(inner); }
        public String render() { return "audit(" + inner.render() + ")"; }
    }
    static final class EncryptDecorator extends ComponentDecorator {
        EncryptDecorator(Component inner) { super(inner); }
        public String render() { return "enc(" + inner.render() + ")"; }
    }
    public static void main(String[] args) {
        Component base = new PlainMessage();
        System.out.println("base=" + base.render());
        System.out.println("audit=" + new AuditDecorator(base).render());
        System.out.println("encrypted=" + new EncryptDecorator(base).render());
        System.out.println("stacked=" + new AuditDecorator(new EncryptDecorator(base)).render());
    }
}
