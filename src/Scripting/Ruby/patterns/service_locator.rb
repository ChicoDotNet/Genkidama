# frozen_string_literal: true

module RubyPatterns
  module ServiceLocator
    module_function

    def run
      services = { clock: -> { '12:00' } }
      raise 'Service Locator failed' unless services.fetch(:clock).call == '12:00'
      true
    end
  end
end

RubyPatterns::ServiceLocator.run if $PROGRAM_NAME == __FILE__
