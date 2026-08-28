# frozen_string_literal: true

module RubyPatterns
  module Microservices
    module_function

    def run
      inventory = ->(sku) { { sku: sku, available: true } }
      order = ->(sku) { inventory.call(sku)[:available] }
      raise 'Microservices failed' unless order.call('A-1')
      true
    end
  end
end

RubyPatterns::Microservices.run if $PROGRAM_NAME == __FILE__
