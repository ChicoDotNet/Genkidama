trait Device {
  def turnOn: String
  def mute: String
}

final case class NamedDevice(name: String) extends Device {
  override def turnOn: String = s"$name:on"
  override def mute: String = s"$name:muted"
}

sealed trait Remote {
  def execute(device: Device): String
}

case object BasicRemote extends Remote {
  override def execute(device: Device): String = device.turnOn
}

case object MuteRemote extends Remote {
  override def execute(device: Device): String = device.mute
}

object Bridge {
  def main(args: Array[String]): Unit = {
    val tv = NamedDevice("TV")
    val radio = NamedDevice("Radio")
    println(s"basic-tv=${BasicRemote.execute(tv)}")
    println(s"basic-radio=${BasicRemote.execute(radio)}")
    println(s"mute-tv=${MuteRemote.execute(tv)}")
    println(s"mute-radio=${MuteRemote.execute(radio)}")
  }
}
