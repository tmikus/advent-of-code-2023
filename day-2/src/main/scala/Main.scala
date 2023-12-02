import scala.io.Source
import scala.math._

val MAX_RED = 12
val MAX_GREEN = 13
val MAX_BLUE = 14

class MaxBalls(val red: Int, val green: Int, val blue: Int)

class GameLine(val number: Int, subsets: List[Subset]) {
  def idValid: Boolean = {
    subsets.forall(_.isValid)
  }

  def getGamePower: Int = {
    val maxBalls = getMaxBalls
    maxBalls.red * maxBalls.green * maxBalls.blue
  }

  private def getMaxBalls: MaxBalls = {
    subsets.foldLeft(MaxBalls(0, 0, 0))((maxBalls, subset) => {
      MaxBalls(
        max(maxBalls.red, subset.red),
        max(maxBalls.green, subset.green),
        max(maxBalls.blue, subset.blue)
      )
    })
  }
}

object GameLine {
  def parse(line: String): GameLine = {
    val lineParts = line.trim.split(':')
    val number = parseGameNumber(lineParts.apply(0))
    val subsets = parseSubsets(lineParts.apply(1))
    GameLine(number, subsets)
  }

  private def parseGameNumber(line: String): Int = {
    line.trim.substring("Game ".length).toInt
  }

  private def parseSubsets(line: String): List[Subset] = {
    line.trim.split(';').iterator.map(Subset.parse).toList
  }
}

class Subset(val red: Int, val green: Int, val blue: Int) {
  def isValid: Boolean = {
    red <= MAX_RED && green <= MAX_GREEN && blue <= MAX_BLUE
  }
}

object Subset {
  def parse(line: String): Subset = {
    line.trim.split(',').map(parseColor).foldLeft(Subset(0, 0, 0))((subset, colorCount) => {
      colorCount match {
        case (count, Color.Red) => Subset(subset.red + count, subset.green, subset.blue)
        case (count, Color.Green) => Subset(subset.red, subset.green + count, subset.blue)
        case (count, Color.Blue) => Subset(subset.red, subset.green, subset.blue + count)
      }
    })
  }
}

enum Color:
  case Red, Green, Blue

def parseColor(line: String): (Int, Color) = {
  val lineParts = line.trim.split(' ')
  val count = lineParts.apply(0).toInt
  val color = lineParts.apply(1) match {
    case "red" => Color.Red
    case "green" => Color.Green
    case _ => Color.Blue
  }
  (count, color)
}

def parseGameLines: List[GameLine] = {
  readInputLines.map(GameLine.parse).toList
}

def readInputLines: Iterator[String] = {
  Source.fromFile("input.txt").getLines()
}

@main def main: Unit = {
  val gameLines = parseGameLines
  val result = gameLines.filter(_.idValid).map(_.number).sum
  println(result)

  val resultPart2 = gameLines.map(_.getGamePower).sum
  println(resultPart2)
}
