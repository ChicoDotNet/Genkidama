# frozen_string_literal: true

module RubyPatterns
  module Memento
    module_function

    def run
      state = { text: 'draft' }
      snapshot = state.dup.freeze
      state[:text] = 'edited'
      state.replace(snapshot)
      raise 'Memento failed' unless state[:text] == 'draft'
      true
    end
  end
end

RubyPatterns::Memento.run if $PROGRAM_NAME == __FILE__
