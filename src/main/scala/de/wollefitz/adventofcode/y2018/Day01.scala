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

object Day01 extends App {
  val logger = Logger("Day01")
  val lines = Source.fromResource("de/wollefitz/adventofcode/y2018/Day01.input").getLines.toList.map(_.toInt)

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
