class ServiceProfile
  property name : String
  getter features : Array(String)

  def initialize(@name : String, features : Array(String))
    @features = features.dup
  end

  def clone_profile : ServiceProfile
    ServiceProfile.new(@name, @features)
  end

  def describe : String
    "#{@name}: #{@features.join(",")}"
  end
end

original = ServiceProfile.new("orders", ["metrics"])
canary = original.clone_profile
canary.name = "orders-canary"
canary.features << "tracing"

puts "original=#{original.describe}"
puts "clone=#{canary.describe}"
