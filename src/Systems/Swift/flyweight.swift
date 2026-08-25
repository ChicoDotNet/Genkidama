final class TextStyle {
    let font: String
    let size: Int
    let color: String

    init(font: String, size: Int, color: String) {
        self.font = font
        self.size = size
        self.color = color
    }
}

final class StyleFactory {
    private var styles: [String: TextStyle] = [:]

    func get(font: String, size: Int, color: String) -> TextStyle {
        let key = "\(font)|\(size)|\(color)"
        if let style = styles[key] { return style }
        let style = TextStyle(font: font, size: size, color: color)
        styles[key] = style
        return style
    }

    var count: Int { styles.count }
}

let factory = StyleFactory()
let red1 = factory.get(font: "Inter", size: 12, color: "red")
let red2 = factory.get(font: "Inter", size: 12, color: "red")
_ = factory.get(font: "Inter", size: 12, color: "blue")
print("styles=\(factory.count);shared=\(red1 === red2);text=ABC")
