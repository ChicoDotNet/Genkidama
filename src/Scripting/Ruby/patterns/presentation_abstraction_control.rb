# frozen_string_literal: true

module RubyPatterns
  module PresentationAbstractionControl
    module_function

    def run
      state = { value: 1 }
      control = ->(delta) { state[:value] += delta }
      presentation = -> { state[:value].to_s }
      control.call(2)
      raise 'Presentation-Abstraction-Control failed' unless presentation.call == '3'
      true
    end
  end
end

RubyPatterns::PresentationAbstractionControl.run if $PROGRAM_NAME == __FILE__
