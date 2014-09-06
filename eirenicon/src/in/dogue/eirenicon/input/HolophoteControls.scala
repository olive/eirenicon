package in.dogue.eirenicon.input

import com.deweyvm.gleany.saving.{ControlName, ControlNameCollection}

class HolophoteControl(descriptor: String) extends ControlName {
  override def name: String = descriptor
}

object HolophoteControls extends ControlNameCollection[HolophoteControl] {
  def fromString(string: String): Option[HolophoteControl] = None
  def makeJoypadDefault: Map[String,String] = Map()
  def makeKeyboardDefault: Map[String,java.lang.Float] = Map()
  def values: Seq[HolophoteControl] = Seq()
}
