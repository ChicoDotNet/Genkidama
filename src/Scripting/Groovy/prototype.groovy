class ServiceProfile {
    String name
    List<String> features

    ServiceProfile cloneProfile() {
        new ServiceProfile(name: name, features: new ArrayList<String>(features))
    }

    String describe() {
        "${name}: ${features.join(',')}"
    }
}

def original = new ServiceProfile(name: 'orders', features: ['metrics'])
def canary = original.cloneProfile()
canary.name = 'orders-canary'
canary.features << 'tracing'

println "original=${original.describe()}"
println "clone=${canary.describe()}"
