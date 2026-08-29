render = fn body -> "<#{body.()}>" end
body = fn -> "sales" end

unless render.(body) == "<sales>" do
  raise "Template"
end
