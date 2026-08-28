# frozen_string_literal: true

require 'monitor'

module RubyPatterns
  module MonitorObject
    module_function

    def run
      counter = Class.new do
        include MonitorMixin
        attr_reader :value

        def initialize
          super
          @value = 0
        end

        def increment
          synchronize { @value += 1 }
        end
      end.new
      threads = Array.new(4) { Thread.new { counter.increment } }
      threads.each(&:join)
      raise 'Monitor Object failed' unless counter.value == 4
      true
    end
  end
end

RubyPatterns::MonitorObject.run if $PROGRAM_NAME == __FILE__
