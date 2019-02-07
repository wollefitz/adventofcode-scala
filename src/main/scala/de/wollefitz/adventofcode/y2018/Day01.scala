package de.wollefitz.adventofcode.y2018

object Day01 extends App {
  val lines = parseInput("de/wollefitz/adventofcode/y2018/Day01.input").map(_.toInt)

  logger.info("Result for Part 1: " + part1(lines))
  logger.info("Result for Part 2: " + part2(lines))

  def part1(list: List[Int]): Int = {
    list match {
      case x :: tail => x + part1(tail)
      case Nil => 0
    }
  }

  def part2(frequencyList: List[Int], set: Set[Int] = Set.empty, sum: Int = 0): Int = {
    sum match {
      case x if set contains x => x
      case _ if frequencyList.isEmpty => part2(lines, set, sum)
      case _ => part2(frequencyList.tail, set + sum, sum + frequencyList.head)
    }
  }
}
