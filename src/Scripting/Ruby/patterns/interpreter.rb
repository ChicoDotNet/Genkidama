# frozen_string_literal: true

module RubyPatterns
  module Interpreter
    module_function

    def run
      env = { x: 4 }
      expr = [:add, [:var, :x], [:lit, 3]]
      evaluate = nil
      evaluate = lambda do |node|
        kind, *args = node
        case kind
        when :lit then args[0]
        when :var then env.fetch(args[0])
        when :add then evaluate.call(args[0]) + evaluate.call(args[1])
        else raise "unknown expression: #{kind}"
        end
      end
      raise 'Interpreter failed' unless evaluate.call(expr) == 7
      true
    end
  end
end

RubyPatterns::Interpreter.run if $PROGRAM_NAME == __FILE__
