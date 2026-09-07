# frozen_string_literal: true

module RubyPatterns
  module Mediator
    class CheckoutMediator
      def initialize
        @colleagues = {}
      end

      def register(name, &receiver)
        @colleagues.fetch(name) { @colleagues[name] = receiver }
      end

      def send(sender, recipient, message)
        @colleagues.fetch(recipient).call(sender, message)
      end
    end

    module_function

    def run
      events = []
      mediator = CheckoutMediator.new
      mediator.register(:inventory) { |sender, message| events << "inventory<-#{sender}:#{message}" }
      mediator.register(:payment) { |sender, message| events << "payment<-#{sender}:#{message}" }

      mediator.send(:payment, :inventory, :paid)
      mediator.send(:inventory, :payment, :reserved)

      expected = ['inventory<-payment:paid', 'payment<-inventory:reserved']
      raise 'Mediator failed' unless events == expected

      true
    end
  end
end

RubyPatterns::Mediator.run if $PROGRAM_NAME == __FILE__
