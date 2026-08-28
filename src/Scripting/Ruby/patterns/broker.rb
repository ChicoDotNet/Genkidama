# frozen_string_literal: true

module RubyPatterns
  module Broker
    module_function

    def run
      handlers = { price: ->(_sku) { 9 } }
      request = ->(topic, payload) { handlers.fetch(topic).call(payload) }
      raise 'Broker failed' unless request.call(:price, 'A') == 9
      true
    end
  end
end

RubyPatterns::Broker.run if $PROGRAM_NAME == __FILE__
