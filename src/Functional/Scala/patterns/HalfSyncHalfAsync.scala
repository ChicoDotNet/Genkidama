object HalfSyncHalfAsyncExample { def run:Boolean=List("job-1","job-2","job-3").map("done:"+_).mkString(">")=="done:job-1>done:job-2>done:job-3" }
