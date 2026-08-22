mutable struct ServiceProfile
    name::String
    features::Vector{String}
end

clone_profile(profile::ServiceProfile) = ServiceProfile(profile.name, copy(profile.features))
describe(profile::ServiceProfile) = "$(profile.name): $(join(profile.features, ","))"

original = ServiceProfile("orders", ["metrics"])
canary = clone_profile(original)
canary.name = "orders-canary"
push!(canary.features, "tracing")

println("original=$(describe(original))")
println("clone=$(describe(canary))")
