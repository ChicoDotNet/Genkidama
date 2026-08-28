# frozen_string_literal: true

module RubyPatterns
  module UnitOfWork
    module_function

    def run
      pending = [{ id: 1 }]
      database = []
      database.concat(pending)
      pending.clear
      raise 'Unit of Work failed' unless database == [{ id: 1 }] && pending.empty?
      true
    end
  end
end

RubyPatterns::UnitOfWork.run if $PROGRAM_NAME == __FILE__
