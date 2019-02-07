package de.wollefitz.adventofcode.y2018

import scala.annotation.tailrec

object Day03 extends App {
  val claims = parseInput("de/wollefitz/adventofcode/y2018/Day03.input").map(parseList)

  logger.info("Result for Part 1: " + part1(claims))
  logger.info("Result for Part 2: " + part2(claims))

  def parseList(claim: String): Claim = {
    val pattern = "#(.*?) @ (.*?),(.*?): (.*?)x(.*)".r

    claim match {
      case pattern(id, paddingLeft, paddingTop, width, height) => Claim(id.toInt, paddingLeft.toInt, paddingTop.toInt, width.toInt, height.toInt)
    }
  }

  def part1(implicit claims: List[Claim]): Int = {
      fillFabric(createFabric).flatten.count(c => c > 1)
  }

  case class Claim(id: Int, paddingLeft: Int, paddingTop: Int, width: Int, height: Int)

  private def createFabric(implicit claims: List[Claim]): Array[Array[Int]] = {
    val rows = claims.map(c => c.paddingTop + c.height).max
    val cols = claims.map(c => c.paddingLeft + c.width).max
    Array.ofDim[Int](rows, cols)
  }

  @tailrec def fillFabric(array: Array[Array[Int]])(implicit claims: List[Claim]): Array[Array[Int]] = {
    claims match {
      case claim :: tail =>
        for(i <- claim.paddingTop until claim.paddingTop + claim.height; j <- claim.paddingLeft until claim.paddingLeft + claim.width) {
          array(i)(j) += 1
        }
        fillFabric(array)(tail)
      case Nil => array
    }
  }

  def part2(implicit claims: List[Claim]): Int = {
    filterFabric(fillFabric(createFabric)).head.id
  }

  def filterFabric(array: Array[Array[Int]])(implicit claims: List[Claim]): List[Claim] = {
    claims.filter(claim => noOverlap(array, claim))
  }

  def noOverlap(array: Array[Array[Int]], claim: Claim): Boolean = {
    val sliced = array.slice(claim.paddingTop, claim.paddingTop + claim.height).map(h => h.slice(claim.paddingLeft, claim.paddingLeft + claim.width))
    if (sliced.flatten.sum == claim.height * claim.width) true else false
  }
}
