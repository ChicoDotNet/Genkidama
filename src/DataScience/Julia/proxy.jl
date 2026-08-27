mutable struct DocumentBackend
    fetches::Int
end

function fetch!(backend::DocumentBackend, id::Int)
    backend.fetches += 1
    "doc($id)"
end

mutable struct DocumentProxy
    backend::Union{Nothing, DocumentBackend}
    cache::Dict{Int, String}
end

DocumentProxy() = DocumentProxy(nothing, Dict{Int, String}())

function get_document!(proxy::DocumentProxy, id::Int)
    get!(proxy.cache, id) do
        if isnothing(proxy.backend)
            proxy.backend = DocumentBackend(0)
        end
        fetch!(proxy.backend::DocumentBackend, id)
    end
end

proxy = DocumentProxy()
first = get_document!(proxy, 42)
second = get_document!(proxy, 42)
backend_count = isnothing(proxy.backend) ? 0 : 1
fetches = isnothing(proxy.backend) ? 0 : (proxy.backend::DocumentBackend).fetches
println("backend=$backend_count;fetches=$fetches;first=$first;second=$second")
