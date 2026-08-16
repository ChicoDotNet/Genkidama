defmodule Builder do
  def text_builder do
    %{
      reset: fn -> [] end,
      add_title: fn title, parts -> parts ++ ["# #{title}"] end,
      add_section: fn heading, body, parts -> parts ++ ["## #{heading}", body] end,
      build: fn parts -> Enum.join(parts, "\n") end
    }
  end

  def html_builder do
    %{
      reset: fn -> [] end,
      add_title: fn title, parts -> parts ++ ["<h1>#{title}</h1>"] end,
      add_section: fn heading, body, parts -> parts ++ ["<h2>#{heading}</h2>", "<p>#{body}</p>"] end,
      build: fn parts -> Enum.join(parts, "") end
    }
  end

  def build_availability_report(builder) do
    parts = builder.reset.()
    parts = builder.add_title.("Service status", parts)
    parts = builder.add_section.("Availability", "99.95%", parts)
    builder.build.(parts)
  end
end

IO.puts(Builder.build_availability_report(Builder.text_builder()))
IO.puts("---")
IO.puts(Builder.build_availability_report(Builder.html_builder()))
