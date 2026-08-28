# frozen_string_literal: true

module RubyPatterns
  module ModelViewPresenter
    module_function

    def run
      model = { name: 'Ada' }
      view = {}
      presenter = -> { view[:text] = model[:name].upcase }
      presenter.call
      raise 'Model-View-Presenter failed' unless view[:text] == 'ADA'
      true
    end
  end
end

RubyPatterns::ModelViewPresenter.run if $PROGRAM_NAME == __FILE__
