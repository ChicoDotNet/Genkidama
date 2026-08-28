# frozen_string_literal: true

module RubyPatterns
  module PublishSubscribe
    module_function

    def run
      topics = Hash.new { |hash, key| hash[key] = [] }
      received = []
      topics[:news] << ->(value) { received << value }
      topics[:news].each { |subscriber| subscriber.call('v1') }
      raise 'Publish-Subscribe failed' unless received == ['v1']
      true
    end
  end
end

RubyPatterns::PublishSubscribe.run if $PROGRAM_NAME == __FILE__
