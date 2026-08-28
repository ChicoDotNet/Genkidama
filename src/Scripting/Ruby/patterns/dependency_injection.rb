# frozen_string_literal: true

module RubyPatterns
  module DependencyInjection
    module_function

    def run
      greeter = Struct.new(:clock) do
        def greet = "hello@#{clock.call}"
      end
      raise 'Dependency Injection failed' unless greeter.new(-> { 'noon' }).greet == 'hello@noon'
      true
    end
  end
end

RubyPatterns::DependencyInjection.run if $PROGRAM_NAME == __FILE__
