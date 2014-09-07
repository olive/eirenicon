package in.dogue.eirenicon.modes

import in.dogue.antiqua.graphics.TileRenderer

object Mode {
  def apply[A](up_ : A => Mode,
               dr_ : A => TileRenderer => TileRenderer,
               self_ : A) = new Mode {
    override type T = A
    override val up = up_
    override val dr = dr_
    override val self = self_
  }
}

trait Mode {
  type T
  val up : T => Mode
  val dr : T => TileRenderer => TileRenderer
  val self: T
  def update = up(self)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< dr(self)
  }
}
