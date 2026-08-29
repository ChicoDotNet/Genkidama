null_logger = fn _message -> :ok end

service = fn logger ->
  logger.("run")
  :ok
end

unless service.(null_logger) == :ok do
  raise "NullObject"
end
