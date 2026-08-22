class DecoratorExample {
    static Closure<String> baseComponent() {
        return { -> 'alert' }
    }

    static Closure<String> auditDecorator(Closure<String> component) {
        return { -> "audit(${component.call()})" }
    }

    static Closure<String> encryptDecorator(Closure<String> component) {
        return { -> "enc(${component.call()})" }
    }

    static void main(String[] args) {
        def base = baseComponent()
        def audited = auditDecorator(base)
        def encrypted = encryptDecorator(base)
        def stacked = auditDecorator(encryptDecorator(base))

        println "base=${base.call()}"
        println "audit=${audited.call()}"
        println "encrypted=${encrypted.call()}"
        println "stacked=${stacked.call()}"
    }
}
