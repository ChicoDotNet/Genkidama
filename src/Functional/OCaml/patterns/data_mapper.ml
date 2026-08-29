type user = { name : string }

let map_row name = { name }

let () = assert ((map_row "Ada").name = "Ada")
