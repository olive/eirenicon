package in.dogue.eirenicon.input

import com.deweyvm.gleany.saving.{ControlName, ControlNameCollection}

class EireniconControl(descriptor: String) extends ControlName {
  override def name: String = descriptor
}

object EireniconControls extends ControlNameCollection[EireniconControl] {
  def fromString(string: String): Option[EireniconControl] = None
  def makeJoypadDefault: Map[String,String] = Map()
  def makeKeyboardDefault: Map[String,java.lang.Float] = Map()
  def values: Seq[EireniconControl] = Seq()
}
