object HalfSyncHalfAsyncExample{fun run()=listOf("job-1","job-2","job-3").map{"done:$it"}.joinToString(">") == "done:job-1>done:job-2>done:job-3"}
