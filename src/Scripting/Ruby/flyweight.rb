TextStyle = Data.define(:font, :size, :color)

class StyleFactory
  def initialize
    @styles = {}
  end

  def get(font, size, color)
    key = [font, size, color]
    @styles[key] ||= TextStyle.new(font, size, color).freeze
  end

  def count
    @styles.size
  end
end

factory = StyleFactory.new
red1 = factory.get('Inter', 12, 'red')
red2 = factory.get('Inter', 12, 'red')
factory.get('Inter', 12, 'blue')
puts "styles=#{factory.count};shared=#{red1.equal?(red2)};text=ABC"
