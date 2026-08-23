type Render = proc (): string {.closure.}

proc plain(): string = "alert"

proc audit(inner: Render): Render =
  result = proc (): string = "audit(" & inner() & ")"

proc encrypt(inner: Render): Render =
  result = proc (): string = "enc(" & inner() & ")"

let base: Render = plain
let audited = audit(base)
let encrypted = encrypt(base)
let stacked = audit(encrypt(base))

echo "base=" & base()
echo "audit=" & audited()
echo "encrypted=" & encrypted()
echo "stacked=" & stacked()
