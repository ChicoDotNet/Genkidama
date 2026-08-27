struct TextStyle
    font::String
    size::Int
    color::String
end

mutable struct StyleFactory
    pool::Dict{Tuple{String, Int, String}, TextStyle}
end

StyleFactory() = StyleFactory(Dict{Tuple{String, Int, String}, TextStyle}())

function get_style!(factory::StyleFactory, font::String, size::Int, color::String)
    key = (font, size, color)
    get!(factory.pool, key) do
        TextStyle(font, size, color)
    end
end

styles = StyleFactory()
red1 = get_style!(styles, "Inter", 12, "red")
red2 = get_style!(styles, "Inter", 12, "red")
blue = get_style!(styles, "Inter", 12, "blue")
@assert blue.color == "blue"
shared = red1.font == red2.font && red1.size == red2.size && red1.color == red2.color
println("styles=$(length(styles.pool));shared=$(shared);text=ABC")
