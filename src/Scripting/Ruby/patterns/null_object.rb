# frozen_string_literal: true

module RubyPatterns
  module NullObject
    module_function

    def run
      null_logger = Object.new
      null_logger.define_singleton_method(:log) { |_message| nil }
      service = Struct.new(:logger) do
        def run
          logger.log('run')
          :ok
        end
      end
      raise 'Null Object failed' unless service.new(null_logger).run == :ok
      true
    end
  end
end

RubyPatterns::NullObject.run if $PROGRAM_NAME == __FILE__
