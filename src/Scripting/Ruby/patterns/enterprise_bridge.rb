# frozen_string_literal: true

module RubyPatterns
  module EnterpriseBridge
    module_function

    def run
      channel = Struct.new(:sender) do
        def notify(text) = sender.call(text)
      end
      result = channel.new(->(text) { "sms:#{text}" }).notify('ok')
      raise 'Enterprise Bridge failed' unless result == 'sms:ok'
      true
    end
  end
end

RubyPatterns::EnterpriseBridge.run if $PROGRAM_NAME == __FILE__
