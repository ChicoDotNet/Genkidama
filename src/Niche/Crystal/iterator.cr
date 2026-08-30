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

def run_iterator_example : Bool
  iterator = CursorIterator.new([10, 20, 30])
  visited = [] of Int32
  while value = iterator.next_value
    visited << value
  end

  visited == [10, 20, 30] && iterator.next_value.nil?
end

if PROGRAM_NAME == __FILE__
  raise "iterator contract failed" unless run_iterator_example

  puts "iterator=10,20,30"
end
