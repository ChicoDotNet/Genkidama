# frozen_string_literal: true

module RubyPatterns
  module Mediator
    module_function

    def run
      events = []
      mediator = ->(sender, message) { events << [sender, message] }
      mediator.call(:checkout, :paid)
      raise 'Mediator failed' unless events == [[:checkout, :paid]]
      true
    end
  end
end

RubyPatterns::Mediator.run if $PROGRAM_NAME == __FILE__
