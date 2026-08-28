# frozen_string_literal: true

module RubyPatterns
  module MVC
    module_function

    def run
      model = { count: 0 }
      controller = -> { model[:count] += 1 }
      view = -> { "count=#{model[:count]}" }
      controller.call
      raise 'MVC failed' unless view.call == 'count=1'
      true
    end
  end
end

RubyPatterns::MVC.run if $PROGRAM_NAME == __FILE__
