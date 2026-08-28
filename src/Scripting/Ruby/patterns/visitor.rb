# frozen_string_literal: true

module RubyPatterns
  module Visitor
    module_function

    def run
      number = Struct.new(:value)
      visitor = ->(node) { node.value * 2 }
      raise 'Visitor failed' unless visitor.call(number.new(5)) == 10
      true
    end
  end
end

RubyPatterns::Visitor.run if $PROGRAM_NAME == __FILE__
