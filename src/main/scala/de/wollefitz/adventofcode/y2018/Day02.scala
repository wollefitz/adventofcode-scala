package de.wollefitz.adventofcode.y2018

object Day02 extends App {
  val lines = parseInput("de/wollefitz/adventofcode/y2018/Day02.input")

  logger.info("Result for Part 1: " + part1(lines).toString)
  logger.info("Result for Part 2: " + part2(lines))

  def part1(words: List[String]): Int = {
    lines.count(hasChecksumCharsFor(2)) * lines.count(hasChecksumCharsFor(3))
  }

  def countChars(word: String): Map[Char, Int] = {
    word.groupBy(identity).mapValues(_.length)
  }

  def hasChecksumCharsFor(n: Int)(word: String): Boolean = {
    countChars(word).values.toSet.contains(n)
  }

  def part2(words: List[String]): String = {
    findCorrectBoxes(words.combinations(2))
  }

  def findCorrectBoxes(words: Iterator[List[String]]): String = {
    val combination = words.next()
    val zipped = combination.head.zip(combination(1))

    difference(zipped) match {
      case diff if diff.size == 1 => zipped.filterNot(_ == diff.head).unzip._1.mkString("")
      case _ if words.hasNext => findCorrectBoxes(words.drop(1))
      case _ => "None"
    }
  }

  def difference(charTuples: IndexedSeq[(Char, Char)]): IndexedSeq[(Char, Char)] = {
    charTuples.filter({ case (c1, c2) => c1 != c2 })
  }

}
