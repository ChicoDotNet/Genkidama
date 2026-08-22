base_component() = "alert"
audit_decorator(component::Function) = () -> "audit($(component()))"
encrypt_decorator(component::Function) = () -> "enc($(component()))"

base = base_component
audited = audit_decorator(base)
encrypted = encrypt_decorator(base)
stacked = audit_decorator(encrypt_decorator(base))

println("base=$(base())")
println("audit=$(audited())")
println("encrypted=$(encrypted())")
println("stacked=$(stacked())")
