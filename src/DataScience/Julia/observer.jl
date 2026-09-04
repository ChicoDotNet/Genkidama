module ObserverExample

mutable struct Subject
    observers::Dict{Symbol, Function}
end

Subject() = Subject(Dict{Symbol, Function}())

function subscribe!(subject::Subject, name::Symbol, observer::Function)
    haskey(subject.observers, name) && return false
    subject.observers[name] = observer
    true
end

function unsubscribe!(subject::Subject, name::Symbol)
    haskey(subject.observers, name) || return false
    delete!(subject.observers, name)
    true
end

function publish(subject::Subject, id::Int)
    Dict(name => observer(id) for (name, observer) in subject.observers)
end

function example_passes()
    subject = Subject()
    subscribe!(subject, :audit, id -> "audit:$id") || return false
    subscribe!(subject, :dashboard, id -> "dashboard:$id") || return false
    !subscribe!(subject, :audit, id -> "duplicate:$id") || return false

    first_delivery = publish(subject, 42)
    first_delivery == Dict(:audit => "audit:42", :dashboard => "dashboard:42") || return false

    unsubscribe!(subject, :dashboard) || return false
    !unsubscribe!(subject, :dashboard) || return false

    publish(subject, 43) == Dict(:audit => "audit:43")
end

end

if abspath(PROGRAM_FILE) == @__FILE__
    ObserverExample.example_passes() || error("Julia Observer verification failed")
    println("Julia Observer: passed")
end
