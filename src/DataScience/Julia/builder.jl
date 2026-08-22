mutable struct ReportBuilder
    html::Bool
    parts::Vector{String}
end

ReportBuilder(html::Bool) = ReportBuilder(html, String[])
reset!(builder::ReportBuilder) = empty!(builder.parts)

function add_title!(builder::ReportBuilder, title::String)
    push!(builder.parts, builder.html ? "<h1>$title</h1>" : "# $title")
end

function add_section!(builder::ReportBuilder, heading::String, body::String)
    if builder.html
        append!(builder.parts, ["<h2>$heading</h2>", "<p>$body</p>"])
    else
        append!(builder.parts, ["## $heading", body])
    end
end

build(builder::ReportBuilder) = join(builder.parts, builder.html ? "" : "\n")

function build_availability_report(builder::ReportBuilder)
    reset!(builder)
    add_title!(builder, "Service status")
    add_section!(builder, "Availability", "99.95%")
    build(builder)
end

println(build_availability_report(ReportBuilder(false)))
println("---")
println(build_availability_report(ReportBuilder(true)))
