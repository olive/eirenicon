package in.dogue.eirenicon

import com.deweyvm.gleany.{AssetLoader, GleanyGame}
import in.dogue.eirenicon.input.Controls
import in.dogue.antiqua.graphics.{TileRenderer, Tileset, Renderer}
import scala.util.Random
import in.dogue.eirenicon.modes.{GameMode, Mode, TitleMode}

class Engine {
  val cols = Game.Cols
  val rows = Game.Rows
  val tsize = Game.TileSize
  val rand = new Random(0)
  val m:Mode = {
    GameMode.create(cols, rows, rand).toMode
  }
  var mode:Mode = m
  val ts = new Tileset(16, 16, tsize, tsize, AssetLoader.loadTexture("16x16"))
  val r = new Renderer(cols*tsize, rows*tsize, 1, ts)
  def update() = {
    if (Controls.Escape.justPressed) {
      GleanyGame.exit()
    }
    mode = mode.update
  }
  def draw() = {
    val tr = TileRenderer.create(cols, rows)
    r.render(tr <+< mode.draw)
    ()
  }

}
