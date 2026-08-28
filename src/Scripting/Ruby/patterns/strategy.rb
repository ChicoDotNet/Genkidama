# frozen_string_literal: true

module RubyPatterns
  module Strategy
    module_function

    def run
      choose = ->(values, strategy) { strategy.call(values) }
      raise 'Strategy min failed' unless choose.call([3, 1, 2], ->(items) { items.min }) == 1
      raise 'Strategy max failed' unless choose.call([3, 1, 2], ->(items) { items.max }) == 3
      true
    end
  end
end

RubyPatterns::Strategy.run if $PROGRAM_NAME == __FILE__
