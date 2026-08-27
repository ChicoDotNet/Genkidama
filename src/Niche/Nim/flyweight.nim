type
  TextStyle = ref object
    font: string
    size: int
    color: string

  StyleFactory = object
    pool: seq[TextStyle]

proc get(factory: var StyleFactory; font: string; size: int; color: string): TextStyle =
  for style in factory.pool:
    if style.font == font and style.size == size and style.color == color:
      return style
  result = TextStyle(font: font, size: size, color: color)
  factory.pool.add(result)

var factory: StyleFactory
let red1 = factory.get("Inter", 12, "red")
let red2 = factory.get("Inter", 12, "red")
let blue = factory.get("Inter", 12, "blue")
doAssert blue.color == "blue"
echo "styles=", factory.pool.len, ";shared=", (if red1 == red2: "true" else: "false"), ";text=ABC"
