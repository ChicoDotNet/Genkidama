struct Handler
    name::String
    accepts::Function
end

function route_request(amount::Int, handlers::Vector{Handler})
    visited = String[]
    for handler in handlers
        push!(visited, handler.name)
        if handler.accepts(amount)
            return visited, handler.name
        end
    end
    error("No handler accepted the request")
end

handlers = Handler[
    Handler("faq", amount -> amount <= 50),
    Handler("billing", amount -> amount <= 500),
    Handler("escalation", _ -> true),
]

amount = 250
visited, handled = route_request(amount, handlers)
println("visited=$(join(visited, ">"));handled=$handled;result=refund($amount)")
