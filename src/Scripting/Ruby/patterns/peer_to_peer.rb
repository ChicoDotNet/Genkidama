# frozen_string_literal: true

module RubyPatterns
  module PeerToPeer
    module_function

    def run
      peers = { a: [], b: [] }
      send_message = ->(source, target, message) { peers.fetch(target) << [source, message] }
      send_message.call(:a, :b, 'hello')
      raise 'Peer-to-Peer failed' unless peers[:b] == [[:a, 'hello']]
      true
    end
  end
end

RubyPatterns::PeerToPeer.run if $PROGRAM_NAME == __FILE__
