# frozen_string_literal: true

module RubyPatterns
  module DataMapper
    module_function

    def run
      table = { 1 => { name: 'Ada' } }
      user = Struct.new(:name)
      mapper = ->(row) { user.new(row[:name]) }
      raise 'Data Mapper failed' unless mapper.call(table[1]).name == 'Ada'
      true
    end
  end
end

RubyPatterns::DataMapper.run if $PROGRAM_NAME == __FILE__
