# frozen_string_literal: true

module RubyPatterns
  module Repository
    module_function

    def run
      data = { 1 => { name: 'Ada' } }
      users = Object.new
      users.define_singleton_method(:get) { |id| data.fetch(id) }
      raise 'Repository failed' unless users.get(1)[:name] == 'Ada'
      true
    end
  end
end

RubyPatterns::Repository.run if $PROGRAM_NAME == __FILE__
