package in.dogue.eirenicon.modes

import scala.util.Random
import in.dogue.antiqua.graphics.{TileRenderer, Tile, Rect}
import in.dogue.antiqua.data.{Array2d, CP437}
import com.deweyvm.gleany.graphics.Color
import in.dogue.eirenicon.world.Dungeon
import in.dogue.eirenicon.procgen.BSPDungeon
import com.deweyvm.gleany.data.Recti
import in.dogue.eirenicon.fov.{ShadowCast, ShadowCastJava}
import java.util
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.eirenicon.input.Controls
import in.dogue.eirenicon.Game

object GameMode {
  def create(cols:Int, rows:Int, r:Random) = {
    val (a, b) = new BSPDungeon(r).create(new Recti(0,0,Game.Cols,Game.Rows), 5)
    val d = Dungeon.create(a, b)
    val blank = d.tiles.map{ case _ => false}
    new GameMode(d, Player((5, 5)), List(), blank, blank)
  }
}

case class Player(pos:Cell) {
  val tile = Tile(CP437.$, Color.Black, Color.White)
  def update(isSolid:Cell => Boolean) = {
    val dx = Controls.AxisX.zip(3, 3)
    val dy = Controls.AxisY.zip(3, 3)
    val dd = pos |+| ((dx, dy))
    if (isSolid(dd)) {
      this
    } else {
      Player(dd)
    }
  }
}

case class GameMode private (d:Dungeon, p:Player, lit:List[Cell], memory:Array2d[Boolean], blank:Array2d[Boolean]) {
  def update = {
    val lit = ShadowCast.cast(p.pos, d.cols, d.rows, 9, d.mask)
    val newMem = lit.foldLeft(memory) { case (arr, pt) =>
      arr.updated(pt, true)
    }
    copy(p=p.update(d.isSolid), lit=lit, memory=newMem).toMode
  }
  def draw(tr:TileRenderer):TileRenderer = {
    val mask = lit.foldLeft(blank) { case (b, pt) =>
      val i:Int = pt._1
      val j:Int = pt._2
      b.updated((i, j), true)
    }


    d.tiles.foldLeft(tr) {case (trr, (pt, t)) =>
      val tile = if (mask.get(pt)) {
        t.render
      } else if (memory.get(pt)) {
        t.render.mapFg(_.dim(2))
      } else {
        t.render.mapFg(_.dim(8))
        //Tile(CP437.` `, Color.Black, Color.Black)
      }
      trr <+ (pt, tile)
    } <+ (p.pos, p.tile)
  }
  def toMode:Mode = Mode[GameMode](_.update, _.draw, this)
}
