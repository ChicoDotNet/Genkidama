# frozen_string_literal: true

module RubyPatterns
  module TemplateMethod
    module_function

    def run
      report = Class.new do
        def render = "<#{body}>"
        def body = raise NotImplementedError
      end
      sales = Class.new(report) { def body = 'sales' }
      raise 'Template Method failed' unless sales.new.render == '<sales>'
      true
    end
  end
end

RubyPatterns::TemplateMethod.run if $PROGRAM_NAME == __FILE__
