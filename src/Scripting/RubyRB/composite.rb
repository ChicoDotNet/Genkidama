class FileLeaf
  def initialize(bytes)
    @bytes = bytes
  end

  def size
    @bytes
  end
end

class FolderComposite
  def initialize(*children)
    @children = children
  end

  def add(child)
    @children << child
  end

  def size
    @children.sum(&:size)
  end
end

readme = FileLeaf.new(2)
docs = FolderComposite.new(FileLeaf.new(3), FileLeaf.new(5))
root = FolderComposite.new(readme, docs)

puts "leaf=#{readme.size}"
puts "docs=#{docs.size}"
puts "root=#{root.size}"
