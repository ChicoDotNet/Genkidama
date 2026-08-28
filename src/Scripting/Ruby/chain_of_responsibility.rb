class Handler
  def initialize(name, limit)
    @name = name
    @limit = limit
    @next_handler = nil
  end

  def then_next(handler)
    @next_handler = handler
    handler
  end

  def handle(amount, visited)
    visited << @name
    return @name if amount <= @limit || @next_handler.nil?

    @next_handler.handle(amount, visited)
  end
end

faq = Handler.new('faq', 50)
billing = Handler.new('billing', 500)
escalation = Handler.new('escalation', Float::INFINITY)
faq.then_next(billing).then_next(escalation)

visited = []
handled = faq.handle(250, visited)
puts "visited=#{visited.join('>')};handled=#{handled};result=refund(250)"
