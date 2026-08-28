null_logger=fn _->:ok end; service=fn logger->logger.("run");:ok end; unless service.(null_logger)==:ok,do: raise "NullObject"
