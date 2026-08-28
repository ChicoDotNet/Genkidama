# frozen_string_literal: true

module RubyPatterns
  module ClientServer
    module_function

    def run
      server = ->(request) { { echo: request } }
      client = ->(value) { server.call(value)[:echo] }
      raise 'Client-Server failed' unless client.call('ping') == 'ping'
      true
    end
  end
end

RubyPatterns::ClientServer.run if $PROGRAM_NAME == __FILE__
