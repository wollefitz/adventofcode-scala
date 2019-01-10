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
