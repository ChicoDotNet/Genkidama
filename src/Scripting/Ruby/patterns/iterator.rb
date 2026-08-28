# frozen_string_literal: true

module RubyPatterns
  module Iterator
    module_function

    def run
      countdown = Enumerator.new { |yielder| 3.downto(1) { |n| yielder << n } }
      raise 'Iterator failed' unless countdown.to_a == [3, 2, 1]
      true
    end
  end
end

RubyPatterns::Iterator.run if $PROGRAM_NAME == __FILE__
