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
    val mask = arr.map { case (_, ttype) => ttype == Free}
    Dungeon(tiles, mask)
  }
}

case class Dungeon private (tiles:Array2d[DTile], mask:Array2d[Boolean]) {
  val cols = tiles.cols
  val rows = tiles.rows
  def isSolid(pos:Cell) = !mask.get(pos)
}
