# frozen_string_literal: true

module RubyPatterns
  module ActiveRecord
    module_function

    def run
      table = {}
      user_class = Class.new do
        define_method(:initialize) { |id, name, storage| @id, @name, @storage = id, name, storage }
        define_method(:save) { @storage[@id] = { name: @name } }
      end
      user_class.new(1, 'Ada', table).save
      raise 'Active Record failed' unless table[1][:name] == 'Ada'
      true
    end
  end
end

RubyPatterns::ActiveRecord.run if $PROGRAM_NAME == __FILE__
