package in.dogue.eirenicon.procgen

import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Antiqua.Cell
import in.dogue.antiqua.data.Array2d
import in.dogue.eirenicon.world.{Wall, Solid, Free, TileType}
import scala.util.Random
import in.dogue.antiqua.Antiqua
import Antiqua._
import in.dogue.antiqua.graphics.Rect

sealed trait Orientation
case object Vertical extends Orientation
case object Horizontal extends Orientation

sealed trait BSPTree

case class Tree(o:Orientation, ts:Seq[BSPTree]) extends BSPTree
case class Node(r:Recti) extends BSPTree

class BSPDungeon(rng:Random) {
  type Line = ((Int,Int),(Int,Int))
  def create(r:Recti, n:Int): (Set[Cell], Array2d[TileType]) = {
    val sr = Recti(r.x + 2, r.y + 2, r.width - 2, r.height - 2)
    val tree = doSplit(n, Node(sr))
    val shrunk = shrink(tree)
    val ct = connectAll(shrunk)
    toCollisions(r, shrunk, ct)
  }


  def top(r:Recti):Line = ((r.x, r.y), (r.x+r.width, r.y))
  def bottom(r:Recti):Line = ((r.x, r.y + r.height), (r.x + r.width, r.y + r.height))
  def left(r:Recti):Line = ((r.x, r.y), (r.x, r.y + r.height))
  def right(r:Recti):Line = ((r.x + r.width, r.y), (r.x + r.width, r.y + r.height))

  def nearest(o:Orientation, r1:Recti, r2:Recti):(Line, Line) = o match {
    case Vertical => (right(r1), left(r2))
    case Horizontal => (bottom(r1), top(r2))
  }

  def assembleRects(t:BSPTree):List[Recti] = {
    def helper(acc:List[Recti])(t:BSPTree):List[Recti] = t match {
      case Node(r) => r :: acc
      case Tree(o, ts) => ts.map(helper(acc)).flatten.toList
    }
    helper(List())(t)
  }


  def connectTree(o:Orientation, t1:BSPTree, t2:BSPTree):List[Line] = {
    val n1 = assembleRects(t1)
    val n2 = assembleRects(t2)
    val (r1, r2) = closest(n1, n2)
    val (l1, l2) = nearest(o, r1, r2)
    connect(o, l1, l2)
  }

  def midPoint(p1:Cell, p2:Cell):Cell = {
    ((p1.x + p2.x) / 2, (p1.y + p2.y) / 2)
  }

  def center(r:Recti):Cell = {
    (r.x + r.width/2, r.y + r.height/2)
  }

  def dist(r1:Recti, r2:Recti):Int = {
    val (mx, my) = center(r1)
    val (nx, ny) = center(r2)
    ((mx - nx).sq + (my - ny).sq).toInt
  }


  def connect(o:Orientation, l1:Line, l2:Line):List[Line] = {
    val (p1, p2) = l1
    val (q1, q2) = l2
    val m1@(m1x, m1y) = midPoint(p1, p2)
    val m2@(m2x, m2y) = midPoint(q1, q2)
    val (m3x, m3y) = midPoint(m1, m2)
    o match {
      case Horizontal =>
        if ((m1y - m2y).abs <= 2) {
          List(((m3x,m1y), (m3x,m2y)))
        } else {
          List(((m1x,m1y), (m1x,m3y)),
               ((m1x,m3y), (m2x,m3y)),
               ((m2x,m3y), (m2x,m2y)))
        }
      case Vertical =>
        if ((m1x - m2x) <= 2) {
          List(((m1x,m3y), (m2x,m3y)))
        } else {
          List(((m1x,m1y), (m3x,m1y)),
               ((m3x,m1y), (m3x,m2y)),
               ((m3x,m2y), (m2x,m2y)))
        }
    }
  }
  def closest(rs1:List[Recti], rs2:List[Recti]):(Recti, Recti) = {
    val xs = for (r1 <- rs1; r2 <- rs2) yield (dist(r1, r2), (r1, r2))
    xs.sortBy { case (i, _) => i }.head._2

  }
  def connectAll(tr:BSPTree):List[Line] = {
    def connectHelper(acc:List[Line])(trr:BSPTree):List[Line] = trr match {
      case Tree(o, Node(r1) :: Node(r2) :: ts) =>
        val (l1, l2) = nearest(o, r1, r2)
        connect(o,l1,l2) ++ acc
      case Tree(o, ts@(t1 :: t2 :: _)) =>
        val cs = connectTree(o, t1, t2)
        val rs = ts.map(connectHelper(acc))
        cs ++ rs.flatten
      case z => println(z) ; acc

    }
    connectHelper(List())(tr)
  }

  def shrink(tr:BSPTree):BSPTree = tr match {
    case Node(rect) => Node(shrinkRect(rect))
    case Tree(o, ts) => Tree(o, ts.map(shrink))
  }

  def getInt(i:Int):Int = {
    if (i <= 0) {
      0
    } else {
      rng.nextInt(i)
    }
  }

  def splitNum(i:Int):(Int,Int) = {
    val first = getInt(i)
    (first, i - first)
  }

  def shrinkRect(r:Recti) = {
    val sx = getInt(r.width - 10)
    val sy = getInt(r.height - 10)
    val (sx1, sx2) = splitNum(sx)
    val (sy1, sy2) = splitNum(sy)
    Recti(r.x + sx1, r.y + sy1, r.width - (sx1 + sx2), r.height - (sy1 + sy2))
  }

  def needSplit(tr:BSPTree):Boolean = tr match {
    case Tree(_, ts) => ts.exists(needSplit)
    case Node(r) =>
      val ratio = r.width.toFloat/r.height
      r.width > 10 && r.height > 10 && (ratio > 2 || ratio < 0.5)
  }

  def doSplit(n:Int, tr:BSPTree):BSPTree = {
    if (n < 0 && !needSplit(tr)) {
      tr
    } else {
      val trr = split(tr)
      doSplit(n-1, trr)
    }
  }

  def split(tr:BSPTree):BSPTree = tr match {
    case Tree(o, ts) => Tree(o, ts.map(split))
    case n@Node(r) =>
      val b = rng.nextBoolean()
      val p = rng.nextDouble()*0.33 + 0.33
      val ratio = r.width.toFloat/r.height
      val o = if (ratio > 2) {
        Vertical
      } else if (ratio < 0.5) {
        Horizontal
      } else {
        b.select(Vertical, Horizontal)
      }
      if (r.width < 10 || r.height < 10) {
        return n
      }
      o match {
        case Vertical =>
          val w1 = (r.width * p).toInt
          val w2 = (r.width * (1-p)).toInt - 1
          if (w1 < 3 || w2 < 3) {
            n
          } else {
            val ls = List(Node(Recti(r.x, r.y, w1, r.height)),
                          Node(Recti(r.x + w1 + 1, r.y, w2, r.height)))
            Tree(Vertical, ls)
          }
        case Horizontal =>
          val h1 = (r.height * p).toInt
          val h2 = (r.height * (1-p)).toInt - 1
          if (h1 < 3 || h2 < 3) {
            n
          } else {
            val ls = List(Node(Recti(r.x, r.y, r.width, h1)),
                          Node(Recti(r.x, r.y + h1 + 1, r.width, h2)))
            Tree(Horizontal, ls)
          }
      }
  }

  def rectToPts(r:Recti):List[Cell] = {
    (for {
      i <- r.x to r.x + r.width - 1
      j <- r.y to r.y + r.height - 1
    } yield (i, j)).toList
  }
  def lineToPts(l:Line):List[Cell] = {
    val ((x1, y1), (x2, y2)) = l
    val x0 = math.min(x1, x2)
    val y0 = math.min(y1, y2)
    rectToPts(Recti(x0, y0, (x1 - x2).abs + 1, (y1 - y2).abs + 1))
  }

  def toCollisions(r:Recti, tr:BSPTree, ls:List[Line]):(Set[Cell], Array2d[TileType]) = {
    val rects = assembleRects(tr)
    val pts = (ls.map(lineToPts) ++ rects.map(rectToPts)).flatten
    val base = Array2d.tabulate(r.width - r.x, r.height - r.y) { case _ => true }
    val result = pts.foldLeft(base) { case (arr, pt) =>
      arr.updated(pt, false)
    }
    val fs:List[Cell] = (for(i <- -1 to 1; j <- -1 to 1 if i != 0 || j != 0) yield (i, j)).toList
    def ns(pt:Cell) = fs.map{ q => result.getOption(q |+| pt).getOrElse(false)}
    def f(pt:Cell, b:Boolean) =
      if (!r.contains(pt)) {
        Solid
      } else if (!b) {
        Free
      } else if (ns(pt).exists{d => !d}) {
        Wall
      } else {
        Solid
      }
    (Set(pts:_*), result.map(f))

  }

}


