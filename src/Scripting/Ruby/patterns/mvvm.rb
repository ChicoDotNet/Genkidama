# frozen_string_literal: true

module RubyPatterns
  module MVVM
    module_function

    def run
      model = { first: 'Ada', last: 'Lovelace' }
      view_model = -> { { display_name: "#{model[:first]} #{model[:last]}" } }
      raise 'MVVM failed' unless view_model.call[:display_name] == 'Ada Lovelace'
      true
    end
  end
end

RubyPatterns::MVVM.run if $PROGRAM_NAME == __FILE__
