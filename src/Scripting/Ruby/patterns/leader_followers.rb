# frozen_string_literal: true

module RubyPatterns
  module LeaderFollowers
    module_function

    def run
      events = %w[one two]
      workers = %i[leader follower].each
      handled = events.map { |event| [workers.next, event] }
      expected = [[:leader, 'one'], [:follower, 'two']]
      raise 'Leader / Followers failed' unless handled == expected
      true
    end
  end
end

RubyPatterns::LeaderFollowers.run if $PROGRAM_NAME == __FILE__
