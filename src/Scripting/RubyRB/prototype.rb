class ServiceProfile
  attr_accessor :name, :features

  def initialize(name, features)
    @name = name
    @features = features
  end

  def initialize_copy(source)
    super
    @features = source.features.dup
  end

  def describe
    "#{name}: #{features.join(',')}"
  end
end

original = ServiceProfile.new('orders', ['metrics'])
canary = original.dup
canary.name = 'orders-canary'
canary.features << 'tracing'

puts "original=#{original.describe}"
puts "clone=#{canary.describe}"
