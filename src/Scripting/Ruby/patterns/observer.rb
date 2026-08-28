# frozen_string_literal: true

module RubyPatterns
  module Observer
    module_function

    def run
      seen = []
      subscribers = [->(event) { seen << event }]
      subscribers.each { |subscriber| subscriber.call(:changed) }
      raise 'Observer failed' unless seen == [:changed]
      true
    end
  end
end

RubyPatterns::Observer.run if $PROGRAM_NAME == __FILE__
