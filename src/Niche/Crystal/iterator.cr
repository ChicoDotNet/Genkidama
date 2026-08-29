class CursorIterator
  def initialize(@values : Array(Int32))
    @index = 0
  end

  def next_value : Int32?
    return nil if @index >= @values.size
    value = @values[@index]
    @index += 1
    value
  end
end

iterator = CursorIterator.new([10, 20, 30])
visited = [] of Int32
while value = iterator.next_value
  visited << value
end

raise "iterator contract failed" unless visited == [10, 20, 30] && iterator.next_value.nil?
puts "iterator=#{visited.join(',')}"
