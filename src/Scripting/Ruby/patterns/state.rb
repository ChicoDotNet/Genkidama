# frozen_string_literal: true

module RubyPatterns
  module State
    module_function

    def run
      door = Object.new
      door.instance_variable_set(:@state, :closed)
      door.define_singleton_method(:toggle) { @state = @state == :closed ? :open : :closed }
      door.toggle
      raise 'State failed' unless door.instance_variable_get(:@state) == :open
      true
    end
  end
end

RubyPatterns::State.run if $PROGRAM_NAME == __FILE__
