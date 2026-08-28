document = %{title: "One"}
plain_view = fn value -> value.title end
upper_view = fn value -> String.upcase(value.title) end

unless {plain_view.(document), upper_view.(document)} == {"One", "ONE"} do
  raise "DocumentView"
end
