class Registry
  @@instance : Registry? = nil

  getter count = 0

  private def initialize
  end

  def self.instance : Registry
    @@instance ||= new
  end

  def increment
    @count += 1
  end
end

first = Registry.instance
second = Registry.instance
first.increment
puts "same=#{first.same?(second)}"
puts "count=#{second.count}"
