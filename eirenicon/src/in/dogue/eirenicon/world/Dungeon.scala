package in.dogue.eirenicon.world

import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import in.dogue.antiqua.data.{CP437, Array2d}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Antiqua
import Antiqua._

object Dungeon {
  def create(pts:Set[Cell], arr:Array2d[TileType]) = {
    val tiles = arr.map {case (pt, ttype) =>
      val tile = ttype match {
        case Wall => Tile(CP437.`#`, Color.Black, Color.White)
        case Solid => Tile(CP437.` `, Color.Black, Color.Black)
        case Free => Tile(CP437.`.`, Color.Black, Color.White)
      }
      DTile(tile, ttype)
    }
    Dungeon(tiles)
  }
}

case class Dungeon private (tiles:Array2d[DTile]) {
  def draw(tr:TileRenderer):TileRenderer = {
    tiles.foldLeft (tr) {case (tr, (pp, t)) =>
      tr <+< t.draw(pp)
    }
  }
}
