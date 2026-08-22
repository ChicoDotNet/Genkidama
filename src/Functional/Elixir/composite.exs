defmodule Composite do
  def file(bytes), do: {:file, bytes}
  def folder(children), do: {:folder, children}

  def size({:file, bytes}), do: bytes
  def size({:folder, children}), do: Enum.reduce(children, 0, fn child, total -> total + size(child) end)
end

readme = Composite.file(2)
docs = Composite.folder([Composite.file(3), Composite.file(5)])
root = Composite.folder([readme, docs])

IO.puts("leaf=#{Composite.size(readme)}")
IO.puts("docs=#{Composite.size(docs)}")
IO.puts("root=#{Composite.size(root)}")
