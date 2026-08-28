module HalfSyncHalfAsyncExample
let run ()=["job-1";"job-2";"job-3"]|>List.map(fun j->$"done:{j}")|>String.concat ">"|>(=)"done:job-1>done:job-2>done:job-3"
