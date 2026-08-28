# frozen_string_literal: true

module RubyPatterns
  module MessageBus
    module_function

    def run
      bus = Hash.new { |hash, key| hash[key] = [] }
      seen = []
      bus[:paid] << ->(value) { seen << value }
      bus[:paid].each { |handler| handler.call(42) }
      raise 'Message Bus failed' unless seen == [42]
      true
    end
  end
end

RubyPatterns::MessageBus.run if $PROGRAM_NAME == __FILE__
