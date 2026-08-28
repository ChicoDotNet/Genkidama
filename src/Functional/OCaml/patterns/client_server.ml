let server request = request
let client value = server value

let () = assert (client "ping" = "ping")
