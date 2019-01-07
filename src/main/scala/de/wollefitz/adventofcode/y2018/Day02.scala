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

import com.typesafe.scalalogging.Logger

import scala.io.Source

object Day02 extends App {

  val logger = Logger("Day02")
  val lines = Source.fromResource("de/wollefitz/adventofcode/y2018/Day02.input").getLines.toList

  logger.info("Result for Part 1: " + part1(lines).toString)

  def part1(words: List[String]): Int = {
    lines.count(hasChecksumCharsFor(2)) * lines.count(hasChecksumCharsFor(3))
  }

  def countChars(word: String): Map[Char, Int] = {
     word.groupBy(identity).mapValues(_.length)
  }

  def hasChecksumCharsFor(n: Int)(word: String): Boolean = {
    countChars(word).values.toSet.contains(n)
  }

}
