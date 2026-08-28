# frozen_string_literal: true

module RubyPatterns
  module DocumentView
    module_function

    def run
      document = { title: 'One' }
      view_a = -> { document[:title] }
      view_b = -> { document[:title].upcase }
      raise 'Document-View failed' unless [view_a.call, view_b.call] == ['One', 'ONE']
      true
    end
  end
end

RubyPatterns::DocumentView.run if $PROGRAM_NAME == __FILE__
