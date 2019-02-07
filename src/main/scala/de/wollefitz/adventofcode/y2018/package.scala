package de.wollefitz.adventofcode

import com.typesafe.scalalogging.Logger

import scala.io.Source

package object y2018 {
  val logger = Logger("AoC 2018")

  def parseInput(resourcePath: String): List[String] = {
    Source.fromResource(resourcePath).getLines.toList
  }
}
