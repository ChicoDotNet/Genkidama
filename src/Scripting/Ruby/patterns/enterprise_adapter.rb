# frozen_string_literal: true

module RubyPatterns
  module EnterpriseAdapter
    module_function

    def run
      legacy = ->(cents) { cents }
      adapter = ->(amount) { legacy.call((amount * 100).round) }
      raise 'Enterprise Adapter failed' unless adapter.call(12.34) == 1234
      true
    end
  end
end

RubyPatterns::EnterpriseAdapter.run if $PROGRAM_NAME == __FILE__
