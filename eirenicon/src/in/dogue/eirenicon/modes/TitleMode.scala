package in.dogue.eirenicon.modes

import in.dogue.antiqua.graphics.{Tile, Rect, TileRenderer}
import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color

object TitleMode {
  def create(cols:Int, rows:Int, r:Random) = {
    val rect = Rect.createPlain(cols, rows, Tile(CP437.`#`, Color.Black, Color.White))
    new TitleMode(rect)
  }
}

class TitleMode(rect:Rect) {
  def update = this.toMode
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< rect.draw((0,0))
  }
  def toMode:Mode = Mode[TitleMode](_.update, _.draw, this)
}
