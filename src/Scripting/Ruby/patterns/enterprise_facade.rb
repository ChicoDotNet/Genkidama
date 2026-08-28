# frozen_string_literal: true

module RubyPatterns
  module EnterpriseFacade
    module_function

    def run
      stock = -> { true }
      charge = -> { :paid }
      checkout = -> { stock.call ? charge.call : :sold_out }
      raise 'Enterprise Facade failed' unless checkout.call == :paid
      true
    end
  end
end

RubyPatterns::EnterpriseFacade.run if $PROGRAM_NAME == __FILE__
