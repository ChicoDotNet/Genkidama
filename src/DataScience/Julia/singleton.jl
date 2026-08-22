const REGISTRY = Ref(0)

registry_instance() = REGISTRY

first = registry_instance()
second = registry_instance()
first[] += 1

println("same=$(first === second)")
println("count=$(second[])")
