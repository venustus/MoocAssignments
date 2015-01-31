package simulations

import math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val deathRate = 0.25

    val maxWaitingTime = 5

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = ((1 to population) map ((i) => new Person(i))).toList
  for {i <- 1 to (population * prevalenceRate).toInt} persons(i).infected = true
  persons foreach ((p) => afterDelay(0)({ p.takeAction }))

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def isVisiblyInfectious = (sick || dead) && !immune

    def moveToRoom(room: (Int, Int)) = {
      room match {
        case (r, c) => row = r; col = c;
        case _ =>
      }
      if(!infected && !immune) {
        val infectiousPersonsInCurrentRoom = persons filter ((p) => {
          (p.row, p.col) match {
            case `room` => p.infected
            case _ => false
          }
        })
        if (infectiousPersonsInCurrentRoom.size > 0 && random < transmissibilityRate) {
          infected = true
        }
      }
    }

    def evaluateAndMove() = {
      val left: (Int, Int) = (row, (col - 1 + roomColumns) % roomColumns)
      val top: (Int, Int) = ((row - 1 + roomRows) % roomRows, col)
      val right: (Int, Int) = (row, (col + 1) % roomColumns)
      val bottom: (Int, Int) = ((row + 1) % roomRows, col)

      val allRooms = Set(left, top, right, bottom)
      val availableRooms = (allRooms /: persons) ((acc, p) => {
        if(p.isVisiblyInfectious && (allRooms contains (p.row, p.col))) (acc - ((p.row, p.col))) else acc
      })
      if(!availableRooms.isEmpty) {
        val chosenRoom = randomBelow(availableRooms.size)
        moveToRoom(availableRooms.toList(chosenRoom))
      }
    }

    def takeAction(): Unit = {
      if(!dead) {
        afterDelay(randomBelow(maxWaitingTime) + 1) {
          evaluateAndMove()
          afterDelay(0){ takeAction() }
        }
        if(infected) {
          afterDelay(6)({ sick = true })
          afterDelay(14)({ if(random < deathRate) dead = true })
          afterDelay(16)({ if(!dead) { immune = true; sick = false }})
          afterDelay(18)({ if(!dead) { immune = false; infected = false; sick = false }})
        }
      }
    }
  }
}
