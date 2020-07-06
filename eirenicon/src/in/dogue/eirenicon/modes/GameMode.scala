package in.dogue.eirenicon.modes

import scala.util.Random
import in.dogue.antiqua.graphics.{Rect, Tile, TileRenderer}
import in.dogue.antiqua.data.{Array2d, CP437, Direction}
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

case class Soldier(pos:Cell, age:Int) {
  val tile = Tile(CP437.S, Color.Black, Color.Green)
  def update(isSolid:Cell => Boolean):Soldier = {
    val r: Random = new Random()
    val newpos =
      if (age % 30 == 0) {
        val potential = pos-->Direction.All(r.nextInt(4))
        if (isSolid(potential)) {
          pos
        } else {
          potential
        }
      } else {
        pos
      }

    Soldier(newpos, age+1)
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (pos, tile)
  }
}

case class Explorer(pos:Cell, visionRadius:Int, map:Map, age:Int) {
  val tile = Tile(CP437.E, Color.Black, Color.Yellow)
  def update(isSolid:Cell => Boolean, oracle:Array2d[Boolean]):Explorer = {
    val r: Random = new Random()
    val newpos =
      if (age % 30 == 0) {
        val potential = pos-->Direction.All(r.nextInt(4))
        if (isSolid(potential)) {
          pos
        } else {
          potential
        }
      } else {
        pos
      }
    val newMap =
      if (!map.isExplored(pos)) {
        map.explore(newpos, visionRadius, oracle)
      } else {
        map
      }
    copy(map=newMap, age=age+1, pos=newpos)
  }
}

case class DistanceQueue[A](items:List[(Int,Set[A])]) {
  def isEmpty = items.isEmpty
  def pop:(Option[(Int,Set[A])],DistanceQueue[A]) = {
    items match {
      case x +: xs => (x.some,DistanceQueue(xs))
      case _ => (None,DistanceQueue(items))
    }
  }

  def insert(a:A, ord:Int):DistanceQueue[A] = {
    def innerInsert(a:A, ord:Int, rest:List[(Int,Set[A])]):List[(Int,Set[A])] = {
      rest match {
        ///int, Vector[A] +: List[(Int,Vector[A])]
        case (xord,xv) :: xs if xord < ord =>
          (ord, Set(a)) :: (xord,xv) :: xs
        case (xord,xv) :: xs if xord == ord =>
          (xord,xv + a) :: xs
        case x :: xs =>
          x :: innerInsert(a, ord, xs)
        case _ =>
          List((ord, Set(a)))
      }
    }
    DistanceQueue(innerInsert(a, ord, items))
  }
  override def toString:String = {
    var s:String = ""
    items.foreach { case (distance, set) =>
      s += distance + ": " + set.toString + "\n"
    }
    s
  }
}

object MapState {
  case object Unexplored extends MapState {
    override def isTraversable = true
    override def isExplored = false
    override def toString:String = "_"
  }
  case object Wall extends MapState {
    override def isTraversable = false
    override def isExplored = true
    override def toString:String = "X"
  }
  case object Free extends MapState {
    override def isTraversable = true
    override def isExplored = true
    override def toString:String = "."
  }
}

sealed trait MapState {
  def isTraversable:Boolean
  def isExplored:Boolean
}

object Map {
  def initialize(cells:Array2d[MapState], origin:Cell):Map = {
    val (oi,oj) = origin
    def getDistance(cell:Cell, state:MapState):Int = {
      val (i,j) = cell
      val distance = Math.abs(oi - i) + Math.abs(oj - j)
      distance
    }
    val distance: Array2d[Int] = cells.map(getDistance)
    //groupBy[K](f:T => K): Map[K, Vector[T]]
    //Cell,Int
    val closest: Predef.Map[Int, Seq[((Int, Int), Int)]] = distance.flatten.groupBy(_._2)
    val keys = closest.keySet.toList.sorted
    def stripDistance[A,B](v:Set[(A,B)]):Set[A] = v.map(_._1)
    val r: List[(Int, Set[Cell])] = keys.map(k => (k,stripDistance(closest(k).toSet)))
    Map(origin, cells.map {case _ => false}, cells,distance, DistanceQueue(List()))
  }
}
/*luki:I'd do an array of stacks
the index in the array is the distance and the stack is where the coords go
ofc that'd need separate storage for if a coordinate has been visited*/

//go to farthest point, explore local points until all local points
// have a distance value of N less than the most distant point known

//any tile that touches an unexplored tile
case class Map (origin:Cell,seen:Array2d[Boolean], cells:Array2d[MapState], distance:Array2d[Int], closest:DistanceQueue[Cell]) {
  def isExplored(p:Cell):Boolean = {
    seen.get(p)
  }
  def explore(p:Cell, radius:Int, oracle:Array2d[Boolean]):Map = {

    val lit: Seq[Cell] = p +: ShadowCast.cast(p, oracle.cols, oracle.rows, radius, oracle)
    val updated = lit.foldLeft(cells)((arr,p) => {
      val newstate = if (arr.get(p).isTraversable) MapState.Free else MapState.Wall
      arr.updated(p, newstate)
    })

    val close = lit.foldLeft(closest)((queue,p) => {
      val distance = Math.abs(origin._1 - p._1) + Math.abs(origin._2 - p._2)
      queue.insert(p, distance)
    })
    copy(cells=updated, closest=close, seen=seen.updated(p, true))
  }
  def render(tr:TileRenderer):TileRenderer = {
    val tile = Tile(CP437.E, Color.Grey, Color.Grey)
    val tgroup = cells.flatten.filter { case (p, state) =>
      state.isTraversable && state.isExplored
    } map { case (p, state) =>
      (p,tile)
    }
    tr <&& tgroup
  }
}


object GameMode {
  def create(cols:Int, rows:Int, r:Random) = {
    val (a, b) = new BSPDungeon(r).create(Recti(0,0,Game.Cols,Game.Rows), 5)
    val d: Dungeon = Dungeon.create(a, b)
    val blank: Array2d[Boolean] = d.tiles.map{ case _ => false}
    val spawn = (5,5)
    val map = Map.initialize(blank.map{case _ => MapState.Unexplored},spawn)
    new GameMode(d, Player(spawn), Explorer((6,5), 7, map, 0),List(Soldier((5,6),0)), List(), blank, blank)
  }
}

case class GameMode private (d:Dungeon, p:Player, e:Explorer, soldiers:List[Soldier],lit:List[Cell], memory:Array2d[Boolean], blank:Array2d[Boolean]) {
  def update = {
    val lit = ShadowCast.cast(p.pos, d.cols, d.rows, 9, d.mask)
    val newMem = lit.foldLeft(memory) { case (arr, pt) =>
      arr.updated(pt, true)
    }
    copy(p=p.update(d.isSolid), lit=lit, memory=newMem, e=e.update(d.isSolid, d.mask), soldiers=soldiers.map(_.update(d.isSolid))).toMode
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val mask = lit.foldLeft(blank) { case (b, pt) =>
      val i:Int = pt._1
      val j:Int = pt._2
      b.updated((i, j), true)
    }
    val stiles:TileGroup = soldiers.map(s => (s.pos, s.tile))

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
    } <+ (p.pos, p.tile) <++ stiles <+ (e.pos, e.tile) <+< e.map.render
  }

  def toMode:Mode = Mode[GameMode](_.update, _.draw, this)
}
