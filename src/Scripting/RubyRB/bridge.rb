class Device
  def initialize(name)
    @name = name
  end

  def turn_on = "#{@name}:on"
  def mute = "#{@name}:muted"
end

class Remote
  def initialize(device)
    @device = device
  end
end

class BasicRemote < Remote
  def execute = @device.turn_on
end

class MuteRemote < Remote
  def execute = @device.mute
end

tv = Device.new('TV')
radio = Device.new('Radio')
puts "basic-tv=#{BasicRemote.new(tv).execute}"
puts "basic-radio=#{BasicRemote.new(radio).execute}"
puts "mute-tv=#{MuteRemote.new(tv).execute}"
puts "mute-radio=#{MuteRemote.new(radio).execute}"
