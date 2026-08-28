# frozen_string_literal: true

module RubyPatterns
  module ObjectPool
    module_function

    def run
      pool = [{ id: 1 }]
      item = pool.shift
      pool << item
      raise 'Object Pool failed' unless pool.first.equal?(item)
      true
    end
  end
end

RubyPatterns::ObjectPool.run if $PROGRAM_NAME == __FILE__
