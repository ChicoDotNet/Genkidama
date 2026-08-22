local function text_builder()
  local parts = {}
  return {
    reset = function() parts = {} end,
    add_title = function(title) table.insert(parts, "# " .. title) end,
    add_section = function(heading, body)
      table.insert(parts, "## " .. heading)
      table.insert(parts, body)
    end,
    build = function() return table.concat(parts, "\n") end,
  }
end

local function html_builder()
  local parts = {}
  return {
    reset = function() parts = {} end,
    add_title = function(title) table.insert(parts, "<h1>" .. title .. "</h1>") end,
    add_section = function(heading, body)
      table.insert(parts, "<h2>" .. heading .. "</h2>")
      table.insert(parts, "<p>" .. body .. "</p>")
    end,
    build = function() return table.concat(parts) end,
  }
end

local function build_availability_report(builder)
  builder.reset()
  builder.add_title("Service status")
  builder.add_section("Availability", "99.95%")
  return builder.build()
end

print(build_availability_report(text_builder()))
print("---")
print(build_availability_report(html_builder()))
