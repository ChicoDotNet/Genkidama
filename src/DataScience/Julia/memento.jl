# Canonical Julia Memento example for Genkidama.
# The originator owns capture/restoration; the caretaker stores an opaque value snapshot.

struct MementoSnapshot
    title::String
    tags::Tuple{Vararg{String}}
end

mutable struct MementoDocument
    title::String
    tags::Vector{String}
end

save_memento(document::MementoDocument) = MementoSnapshot(document.title, Tuple(document.tags))

function restore_memento!(document::MementoDocument, snapshot::MementoSnapshot)
    document.title = snapshot.title
    document.tags = collect(snapshot.tags)
    document
end

function verify_memento_canonical()
    document = MementoDocument("draft", ["pattern"])
    snapshot = save_memento(document)

    document.title = "published"
    push!(document.tags, "edited")

    @assert snapshot == MementoSnapshot("draft", ("pattern",))
    @assert document.title == "published"
    @assert document.tags == ["pattern", "edited"]

    restore_memento!(document, snapshot)
    @assert document.title == "draft"
    @assert document.tags == ["pattern"]

    # Restored mutable state must not alias the caretaker snapshot.
    document.tags[1] = "restored"
    @assert snapshot.tags == ("pattern",)
end

if abspath(PROGRAM_FILE) == @__FILE__
    verify_memento_canonical()
    println("Julia Memento: passed")
end
