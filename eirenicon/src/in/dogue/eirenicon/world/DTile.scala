package in.dogue.eirenicon.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.Antiqua.Cell

sealed trait TileType
case object Free extends TileType
case object Solid extends TileType
case object Wall extends TileType



case class DTile(t:Tile, tt:TileType) {

  def render = t
}
