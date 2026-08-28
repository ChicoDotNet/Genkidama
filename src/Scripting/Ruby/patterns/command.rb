# frozen_string_literal: true

module RubyPatterns
  module Command
    module_function

    def run
      balance = 100
      commands = [-> { balance += 50 }, -> { balance -= 20 }]
      commands.each(&:call)
      raise 'Command failed' unless balance == 130
      true
    end
  end
end

RubyPatterns::Command.run if $PROGRAM_NAME == __FILE__
