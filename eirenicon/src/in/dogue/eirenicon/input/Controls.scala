package in.dogue.eirenicon.input

import com.deweyvm.gleany.input.triggers.{MouseTrigger, TriggerAggregate, KeyboardTrigger, JoypadTrigger}
import com.deweyvm.gleany.input.{AxisControl, Control, JoypadButton}
import com.badlogic.gdx.Input
import scala.collection.mutable.ArrayBuffer
import in.dogue.antiqua.Antiqua
import Antiqua._

object Controls {
  val All = ArrayBuffer[Control[Boolean]]()
  val Left = makePr(Input.Keys.LEFT, JoypadButton("5"), None)
  val Right = makePr(Input.Keys.RIGHT, JoypadButton("6"), None)
  val Up = makePr(Input.Keys.UP, JoypadButton("6"), None)
  val Down = makePr(Input.Keys.DOWN, JoypadButton("6"), None)
  val Escape = makeKb(Input.Keys.ESCAPE)
  val AxisX = new AxisControl(Left, Right)
  val AxisY = new AxisControl(Up, Down)

  def makeKb(key:Int) = {
    val result = new TriggerAggregate(Seq(new KeyboardTrigger(key)))
    All += result
    result
  }

  def makePr(key:Int, ctrl:JoypadButton, mouse:Option[Int]): TriggerAggregate = {
    val mouseTrigger: Seq[Option[MouseTrigger]] = Seq(mouse.map { i => new MouseTrigger(i)})
    val mt = mouseTrigger.flatten
    val result = new TriggerAggregate(mt ++ Seq(new KeyboardTrigger(key), new JoypadTrigger(ctrl)))
    All += result
    result
  }

  def update() {
    All foreach (_.update())
  }
}
