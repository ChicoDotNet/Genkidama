class TextStyle
  getter font : String
  getter size : Int32
  getter color : String

  def initialize(@font : String, @size : Int32, @color : String)
  end
end

class StyleFactory
  getter pool = {} of String => TextStyle

  def get(font : String, size : Int32, color : String) : TextStyle
    key = "#{font}|#{size}|#{color}"
    @pool[key] ||= TextStyle.new(font, size, color)
  end
end

factory = StyleFactory.new
red1 = factory.get("Inter", 12, "red")
red2 = factory.get("Inter", 12, "red")
blue = factory.get("Inter", 12, "blue")
raise "blue style missing" unless blue.color == "blue"
puts "styles=#{factory.pool.size};shared=#{red1.same?(red2) ? "true" : "false"};text=ABC"
