package org.freemind.practice

import java.io.{File, FileNotFoundException}
import java.util.regex.Pattern


import scala.io.Source


/**
  * The input is like
  * PLACE 0, 0, NORTH
  * FORWARD
  * FORWARD
  * TURN RIGHT
  * FORWARD
  * STATUS
  *
  * All boardgame resource files are under boardgame sub-folder of resources.
  *
  * Need to output, ex "Avator is at (2,1) facing EAST"
  * The board game is 10 x 10 matrix bondary.  You cannot step out of the boundary
  * x dimension is vertical line and y dimension is horizontal line.
  * EAST(0, 1),
  * SOUTH(-1, 0),
  * WEST(0, -1),
  * NORTH(1, 0);
  *
  * Originally it's a question I applied for one previous job which I was accepted.  I wrote in Java when I applied.
  *
  * This is a very good example of Function Programming in Scala.  Make good use of functional programming in Scala:
  * case class, sealed class, case object(essentially java enum) and pattern-match, Option and recursive processing.
  * It's well-organized and easy-read.
  *
  * sling 2019/01/11
  */
object BoardGameSolver {

  val GRID_SIZE = 10

  case class Pos(x: Int, y: Int) {
    require(x >= 0 && x <= 10 && y >= 0 && y <= 10)

    def dx(d: Int): Pos = copy(x = x + d)
    def dy(d: Int): Pos = copy(y = y + d)

    override def toString() = "(" + x + ", " + y + ")"

    def forwardWhileFacing(dir: Direction): Pos =
      try {
        dir match {
          case North => dx(+1)
          case South => dx(-1)
          case East => dy(+1)
          case West => dy(-1)
        }
      }
      catch {
        case e: IllegalArgumentException =>
        println("Intent to forward beyond the grid boundary, IGNORE")
        this
      }
  }

  sealed abstract class Direction {
    val turnRight: Direction
    val turnLeft: Direction

    override def toString = getClass.getSimpleName.init.toUpperCase
  }
  case object North extends Direction {
    val turnRight = East
    val turnLeft = West
  }
  case object South extends Direction {
    val turnRight = West
    val turnLeft = East
  }
  case object East extends Direction {
    val turnRight = South
    val turnLeft = North
  }
  case object West extends Direction {
    val turnRight = North
    val turnLeft = South
  }

  case class State(pos: Pos, dir: Direction) {
    override def toString = "Avator is at "+pos+" facing "+ dir
  }
  val dirMap: Map[String, Direction] = Map("NORTH" -> North, "SOUTH" -> South, "EAST" -> East, "WEST" -> West)
  val stepMap: Map[String, Step] = Map("FORWARD" -> Forward, "TURN RIGHT" -> TurnRight, "TURN LEFT" -> TurnLeft, "STATUS" -> Status)

  sealed abstract class Step {
    def change(curr: State): State
  }
  case object Forward extends Step {
    def change(curr: State) =
      curr.copy(pos = curr.pos.forwardWhileFacing(curr.dir))
  }
  case object TurnRight extends Step {
    def change(curr: State) =
      curr.copy(dir = curr.dir.turnRight)
  }
  case object TurnLeft extends Step {
    def change(curr: State) =
      curr.copy(dir = curr.dir.turnLeft)
  }
  case object Status extends Step {
    def change(curr: State) = curr
  }
  case class Place(pos: Pos, dir: Direction) extends Step {
    def change(curr: State) = State(pos, dir)
  }

  val regex = "PLACE (\\d+), (\\d+), ([A-Z]+)"
  val p: Pattern = Pattern.compile(regex)

  def parsePlace(input: String): Place = {
    val m = p.matcher(input)
    if (m.find) {
      val pos = Pos(m.group(1).toInt, m.group(2).toInt)
      val dir = dirMap.get(m.group(3)) match {
        case Some(d) => d
        case None => throw new IllegalArgumentException(s"Invalid direction: ${m.group(3)}")
      }
      Place(pos, dir)
    }
    else
      throw new IllegalArgumentException(s"Invalid PLACE string: $input")
  }

  def stepIterate(curr: State, l: List[String]): State =
    if (l.isEmpty) curr
    else {
      val stepStr = l.head
      stepMap.get(stepStr) match {
        case Some(step) => stepIterate(step.change(curr), l.tail)
        case None =>  stepIterate(parsePlace(stepStr).change(curr), l.tail)
      }
    }

  def main(args: Array[String]): Unit = {

    try {
      val initialState: State = State(Pos(0,0), North)
      val path = getClass.getResource(args(0))
      val folder = new File(path.getPath)
      if (folder.exists() && folder.isDirectory) {
        folder.listFiles.sortBy(_.getName).foreach {
          f =>
            println("file= " + f.getName)
            val fileSource = Source.fromFile(f)
            println(stepIterate(initialState, fileSource.getLines.toList))
            println()
            fileSource.close()
        }
      }
      else {
        throw new FileNotFoundException(s"Folder ${args(0)} is not found")
      }
    }
    catch {
      case e: Exception =>
        println(s"Could not load read folder ${args(0)} ," + e)
        throw e
    }
  }

}
