package chapter6

import chapter6.State.{get, sequence, transit}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  def machineTransition: Input => Machine => Machine = {
    in =>
      m =>
        in match {
          case _ if m.candies == 0 => m
          case Coin if m.locked => Machine(locked = false, m.candies, m.coins + 1)
          case Turn if !m.locked => Machine(locked = true, m.candies - 1, m.coins)
          case _ => m
        }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs map (transit[Machine] _ compose machineTransition))
      m <- get[Machine]
    } yield (m.candies, m.coins)
  }

  def main(args: Array[String]): Unit = {
    println(simulateMachine(List(Turn, Turn, Coin, Turn)).run(Machine(false, 10, 5)))
  }
}
