import java.util.ArrayList;
import java.util.List;

interface Prototype<T> {
    T copy();
}

final class ServiceProfile implements Prototype<ServiceProfile> {
    private String name;
    private final List<String> features;

    ServiceProfile(String name, List<String> features) {
        this.name = name;
        this.features = new ArrayList<>(features);
    }

    @Override
    public ServiceProfile copy() {
        return new ServiceProfile(name, features);
    }

    void setName(String name) {
        this.name = name;
    }

    void addFeature(String feature) {
        features.add(feature);
    }

    String describe() {
        return name + ": " + String.join(",", features);
    }
}

public final class PrototypeExample {
    public static void main(String[] args) {
        Prototype<ServiceProfile> prototype = new ServiceProfile("orders", List.of("metrics"));
        ServiceProfile original = (ServiceProfile) prototype;
        ServiceProfile canary = prototype.copy();

        canary.setName("orders-canary");
        canary.addFeature("tracing");

        System.out.println("original=" + original.describe());
        System.out.println("clone=" + canary.describe());
    }
}
