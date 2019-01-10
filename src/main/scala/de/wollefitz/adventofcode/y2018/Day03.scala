// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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
    val a = fillFabric(createFabric).flatten
    a.count(c => c > 1)
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
