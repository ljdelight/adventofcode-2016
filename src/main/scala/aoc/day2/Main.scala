package aoc.day2

import scala.io.Source

object Main {


  type Pair = (Int, Int)

  abstract case class Grid(val map: String, val width: Int) {

    def isInBounds(p: Pair): Boolean = {
      val i = p._1
      val j = p._2
      (i >= 0 && i < width) &&
        (j >= 0 && j < width) &&
        !get(p).equals('.')
    }

    def get(p: Pair): Char = {
      map.charAt(p._1 * width + p._2)
    }

    def up(p: Pair): Pair = {
      val moved = (math.max(0, p._1 - 1), p._2)
      if (isInBounds(moved)) moved else p
    }

    def down(p: Pair): Pair = {
      val moved = (math.min(width - 1, p._1 + 1), p._2)
      if (isInBounds(moved)) moved else p
    }

    def left(p: Pair): Pair = {
      val moved = (p._1, math.max(0, p._2 - 1))
      if (isInBounds(moved)) moved else p
    }

    def right(p: Pair): Pair = {
      val moved = (p._1, math.min(width - 1, p._2 + 1))
      if (isInBounds(moved)) moved else p
    }
  }


  class Simple3x3() extends Grid("123456789", 3) {
  }

  class Complex() extends Grid(
    "..1.." +
      ".234." +
      "56789" +
      ".ABC." +
      "..D..", 5
  ) {
  }


  def solve(b: Grid, init: Pair, input: List[String]) = {
    val res = input.foldLeft(("", init))((acc, instructionLine)  => {
      def determineKeyForLine(board: Grid)(line: String, pos: Pair): Pair = {
        val res = line.foldLeft(pos)((p: Pair, thechar: Char) => {
          // println(thechar)
          thechar match {
            case 'U' => board.up(p)
            case 'D' => board.down(p)
            case 'L' => board.left(p)
            case 'R' => board.right(p)
            case _ => p
          }
        })
        // println(res)
        res
      }

      val thekey = determineKeyForLine(b)(instructionLine, acc._2)
      (acc._1 + b.get(thekey).toString, thekey)
    })
    res._1
  }

  def main(args: Array[String]): Unit = {
    val filename = "/aoc/day2/in1.txt"
    val stream = getClass.getResourceAsStream(filename)
    val cleanedInput: List[String] = Source.fromInputStream(stream).getLines().toList

    val part1 = solve(new Simple3x3(), (1, 1), cleanedInput)
    val part2 = solve(new Complex(), (2, 0), cleanedInput)

    println(s"Part 1 solution: $part1")
    println(s"Part 2 solution: $part2")
  }
}
