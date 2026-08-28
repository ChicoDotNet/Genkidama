# frozen_string_literal: true

module RubyPatterns
  module HalfSyncHalfAsync
    module_function

    def run
      incoming = Queue.new
      %w[a b].each { |value| incoming << value }
      completed = []
      completed << incoming.pop.upcase until incoming.empty?
      raise 'Half-Sync / Half-Async failed' unless completed == %w[A B]
      true
    end
  end
end

RubyPatterns::HalfSyncHalfAsync.run if $PROGRAM_NAME == __FILE__
