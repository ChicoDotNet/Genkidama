type node = { value : int }

let visit node = node.value * 2

let () = assert (visit { value = 5 } = 10)
