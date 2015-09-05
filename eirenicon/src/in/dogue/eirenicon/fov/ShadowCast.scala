package in.dogue.eirenicon.fov


import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.data.Array2d

object ShadowCast {
  def cast(pos:Cell, w:Int, h:Int, r:Int, mask:Array2d[Boolean]):List[Cell] = {
    def isSolid(pt:(Integer,Integer)) = {
      val i:Int = pt._1
      val j:Int = pt._2
      !mask.getOption((i, j)).getOrElse(true)
    }
    import scala.collection.JavaConverters._
    val lst = ShadowCastJava.cast(pos.x, pos.y, r, w, h, isSolid).asScala.toList
    lst.map {case (x, y) =>
      val i:Int = x
      val j:Int = y
      (i, j)
    }
  }
}
