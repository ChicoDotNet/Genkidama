require 'singleton'

class Registry
  include Singleton

  attr_reader :count

  def initialize
    @count = 0
  end

  def increment
    @count += 1
  end
end

first = Registry.instance
second = Registry.instance
first.increment

puts "same=#{first.equal?(second)}"
puts "count=#{second.count}"
