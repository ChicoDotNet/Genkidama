# frozen_string_literal: true

module RubyPatterns
  module ActiveObject
    module_function

    def run
      mailbox = Queue.new
      state = []
      mailbox << -> { state << :done }
      mailbox.pop.call
      raise 'Active Object failed' unless state == [:done]
      true
    end
  end
end

RubyPatterns::ActiveObject.run if $PROGRAM_NAME == __FILE__
