package in.dogue.eirenicon.modes

import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Tile, Rect}
import in.dogue.antiqua.data.CP437
import com.deweyvm.gleany.graphics.Color
import in.dogue.eirenicon.world.Dungeon
import in.dogue.eirenicon.procgen.BSPDungeon
import com.deweyvm.gleany.data.Recti

object GameMode {
  def create(cols:Int, rows:Int, r:Random) = {
    val (a, b) = new BSPDungeon(r).create(new Recti(0,0,64,48), 4)
    val d = Dungeon.create(a, b)
    new GameMode(d)
  }
}

class GameMode(d:Dungeon) {
  def update = this.toMode
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< d.draw
  }
  def toMode:Mode = Mode[GameMode](_.update, _.draw, this)
}
