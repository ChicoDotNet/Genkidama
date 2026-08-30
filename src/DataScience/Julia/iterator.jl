struct CursorIterator{T}
    values::Vector{T}
end

Base.IteratorSize(::Type{<:CursorIterator}) = Base.SizeUnknown()
Base.iterate(iterator::CursorIterator, state::Int = 1) =
    state > length(iterator.values) ? nothing : (iterator.values[state], state + 1)

visited = collect(CursorIterator([10, 20, 30]))
visited == [10, 20, 30] || error("iterator contract failed")

if abspath(PROGRAM_FILE) == @__FILE__
    println("iterator=$(join(visited, ','))")
end