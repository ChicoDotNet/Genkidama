class Handler
  getter name : String
  getter limit : Int32
  getter next_handler : Handler?

  def initialize(@name : String, @limit : Int32, @next_handler : Handler? = nil)
  end

  def handle(amount : Int32, visited : Array(String)) : String
    visited << name
    return "handled=#{name};result=refund(#{amount})" if amount <= limit

    successor = next_handler
    return "handled=none;result=rejected" if successor.nil?

    successor.handle(amount, visited)
  end
end

escalation = Handler.new("escalation", Int32::MAX)
billing = Handler.new("billing", 500, escalation)
faq = Handler.new("faq", 50, billing)
visited = [] of String
result = faq.handle(250, visited)
puts "visited=#{visited.join(">")};#{result}"
