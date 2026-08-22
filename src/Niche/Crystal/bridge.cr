abstract class Device
  abstract def power_on : String
  abstract def mute : String
end

class TvDevice < Device
  def power_on : String
    "TV:on"
  end

  def mute : String
    "TV:muted"
  end
end

class RadioDevice < Device
  def power_on : String
    "Radio:on"
  end

  def mute : String
    "Radio:muted"
  end
end

abstract class RemoteControl
  def initialize(@device : Device)
  end

  abstract def activate : String
end

class BasicRemote < RemoteControl
  def activate : String
    @device.power_on
  end
end

class MuteRemote < RemoteControl
  def activate : String
    @device.mute
  end
end

tv = TvDevice.new
radio = RadioDevice.new
puts "basic-tv=#{BasicRemote.new(tv).activate}"
puts "basic-radio=#{BasicRemote.new(radio).activate}"
puts "mute-tv=#{MuteRemote.new(tv).activate}"
puts "mute-radio=#{MuteRemote.new(radio).activate}"
