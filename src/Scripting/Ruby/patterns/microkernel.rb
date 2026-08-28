# frozen_string_literal: true

module RubyPatterns
  module Microkernel
    module_function

    def run
      plugins = { upper: ->(text) { text.upcase } }
      raise 'Microkernel failed' unless plugins.fetch(:upper).call('plugin') == 'PLUGIN'
      true
    end
  end
end

RubyPatterns::Microkernel.run if $PROGRAM_NAME == __FILE__
