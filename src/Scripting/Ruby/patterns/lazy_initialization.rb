# frozen_string_literal: true

module RubyPatterns
  module LazyInitialization
    module_function

    def run
      calls = 0
      lazy = Object.new
      lazy.define_singleton_method(:value) do
        unless instance_variable_defined?(:@value)
          calls += 1
          @value = Object.new
        end
        @value
      end
      first = lazy.value
      second = lazy.value
      raise 'Lazy Initialization failed' unless first.equal?(second) && calls == 1
      true
    end
  end
end

RubyPatterns::LazyInitialization.run if $PROGRAM_NAME == __FILE__
