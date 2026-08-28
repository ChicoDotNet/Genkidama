# frozen_string_literal: true

module RubyPatterns
  module DistributedProxy
    module_function

    def run
      remote = ->(id) { { id: id, name: 'Ada' } }
      proxy = Struct.new(:id, :remote) do
        def name = remote.call(id)[:name]
      end
      raise 'Distributed Proxy failed' unless proxy.new(7, remote).name == 'Ada'
      true
    end
  end
end

RubyPatterns::DistributedProxy.run if $PROGRAM_NAME == __FILE__
