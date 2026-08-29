let inventory sku = (sku, true)
let order sku = snd (inventory sku)

let () = assert (order "A-1")
