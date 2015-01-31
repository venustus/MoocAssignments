package quickcheck

import scala.math.min

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

    def checkSorted(heap: H, prev: A): Boolean = {
        if(isEmpty(heap)) true
        else {
            val curMin = findMin(heap)
            (curMin >= prev) && checkSorted(deleteMin(heap), curMin)
        }
    }

    def getSortedSequence(heap: H): List[A] = {
        if(isEmpty(heap)) List()
        else {
            findMin(heap) :: getSortedSequence(deleteMin(heap))
        }
    }

    property("min1") = forAll { a: Int =>
        val h = insert(a, empty)
        findMin(h) == a
    }

    property("min of two") = forAll { (a: Int, b: Int) =>
        val h = insert(b, insert(a, empty))
        findMin(h) == min(a, b)
    }

    property("insert and delete") = forAll { a: Int =>
        val h = insert(a, empty)
        val h1 = deleteMin(h)
        isEmpty(h1)
    }

    property("extracting sorted sequence from heap") = forAll { h: H =>
        checkSorted(h, Int.MinValue)
    }

    property("min of melding of two heaps") = forAll { (h1: H, h2: H) =>
        if(isEmpty(h1) || isEmpty(h2)) true
        else {
            val min1 = findMin(h1)
            val min2 = findMin(h2)
            val min = findMin(meld(h1, h2))
            min == min1 || min == min2
        }
    }

    property("mysterious prop") = forAll { (h1: H, h2: H) =>
        val h3 = meld(h1, h2)
        val h4 = deleteMin(h1)
        val h5 = insert(findMin(h1), h2)
        val h6 = meld(h4, h5)
        getSortedSequence(h3) == getSortedSequence(h6)
    }

    lazy val genHeap: Gen[H] = for {
        i <- arbitrary[Int]
        h <- oneOf(value(empty), genHeap)
    } yield insert(i, h)

    implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
