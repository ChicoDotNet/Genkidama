type Registry = object
  count: int

var sharedRegistry = Registry(count: 0)

proc instance(): ptr Registry =
  addr sharedRegistry

let first = instance()
let second = instance()
first[].count += 1

echo "same=", if first == second: "true" else: "false"
echo "count=", second[].count
