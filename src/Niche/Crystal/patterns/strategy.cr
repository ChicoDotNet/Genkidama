alias PricingStrategy = Proc(Int32, Int32)

def price(value : Int32, strategy : PricingStrategy) : Int32
  strategy.call(value)
end

def verify_strategy : Bool
  regular = ->(value : Int32) { value }
  vip = ->(value : Int32) { value * 80 // 100 }

  price(100, regular) == 100 && price(100, vip) == 80
end

if PROGRAM_NAME == __FILE__
  raise "Strategy contract failed" unless verify_strategy
  puts "regular=100;vip=80"
end
