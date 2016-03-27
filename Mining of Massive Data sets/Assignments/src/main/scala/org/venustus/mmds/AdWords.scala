package org.venustus.mmds

/**
 * Created by venkat on 15/03/15.
 */
object AdWords {
    def allocateSlots(advertisers: Set[Advertiser], numSlots: Int) = {
        println("Allocating slots to advertisers: " + advertisers + " and for slots: " + numSlots)
        (((Vector[Advertiser](), advertisers) /: (0 until numSlots))((acc, slotNum) => {
            acc match {
                case (allocation, remainingAdvertisers) => {
                    val chosenAdvertiser = remainingAdvertisers maxBy ((a) => a.bid * a.ctr(slotNum))
                    (allocation :+ chosenAdvertiser,
                        remainingAdvertisers - chosenAdvertiser)
                }
            }
        }))._1
    }

    def checkBudgets(advertisers: Set[Advertiser]) = {
        (Set[Advertiser]() /: advertisers)((acc, advertiser) => {
            if(advertiser.budget < (advertiser.ctr min)) acc + advertiser
            else acc
        })
    }

    def getClickCountsForPhase(allocations: Vector[Advertiser], numSlots: Int) = {
        val affordableImpressions = allocations map ((ad) => (ad.budget / ad.bid) / ad.ctr(allocations indexOf(ad)))
        allocations map ((ad) => (ad.ctr(allocations indexOf(ad)) * affordableImpressions.min).toInt)
    }

    def countClickThroughs(advertisers: Set[Advertiser], numSlots: Int, totalClickThroughs: Int) = {
        def executePhase(remainingClickThroughs: Int, remainingAdvertisers: Set[Advertiser], clickThroughMap: Map[Advertiser, Int]): Map[Advertiser, Int] = {
            val slotAllocation = allocateSlots(remainingAdvertisers, numSlots)
            var clickThroughCount = remainingClickThroughs
            var tempAdvertisers = remainingAdvertisers -- slotAllocation
            println(s"Remaining click throughs: ${remainingClickThroughs}, remaining advertisers: ${remainingAdvertisers}")
            println(s"Slot allocation: $slotAllocation")
            if(clickThroughCount <= 0) clickThroughMap
            else {
                val clickCounts = getClickCountsForPhase(slotAllocation, numSlots)
                println(s"Click counts for this phase: $clickCounts")
                if(clickCounts.exists((i) => i != 0)) {
                    val newResult = (clickThroughMap /: (0 until numSlots))((acc, slotNum) => {
                        clickThroughCount = clickThroughCount - clickCounts(slotNum)
                        tempAdvertisers = tempAdvertisers + slotAllocation(slotNum).updated(clickCounts(slotNum))
                        acc + (slotAllocation(slotNum) -> clickCounts(slotNum))
                    })

                    val exiters = checkBudgets(tempAdvertisers)

                    println(s"Calling next phase after exiting advertisers: $exiters")
                    println(s"New result is: $newResult")
                    executePhase(clickThroughCount, tempAdvertisers -- exiters, newResult)
                }
                else clickThroughMap
            }
        }
        executePhase(totalClickThroughs, advertisers, Map[Advertiser, Int]())
    }
}

case class Advertiser(val name: String, val bid: Double, val ctr: Vector[Double], val budget: Double) {

    def updated(clicks: Int) = Advertiser(name, bid, ctr, budget - (bid * clicks))

    override def hashCode: Int = name.hashCode

    override def toString = "(" + name + "," + "%.2f".format(budget) + ")"

    override def equals(other: Any) = other match {
        case that: Advertiser => that.name == name
        case _ => false
    }
}

object Main extends App {
    override def main(args: Array[String]) = {
        val finalClickThroughs = AdWords countClickThroughs (Set(Advertiser("A", 0.1, Vector(0.015, 0.01, 0.005), 1D),
                                        Advertiser("B", 0.09, Vector(0.016, 0.012, 0.006), 2D),
                                        Advertiser("C", 0.08, Vector(0.017, 0.014, 0.007), 3D),
                                        Advertiser("D", 0.07, Vector(0.018, 0.015, 0.008), 4D),
                                        Advertiser("E", 0.06, Vector(0.019, 0.016, 0.01), 5D)), 3, 101)
        println(finalClickThroughs)
    }
}