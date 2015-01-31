import quickcheck.{BinomialHeap, QuickCheckHeap}

object QuickCheckCheck extends QuickCheckHeap with BinomialHeap {

    def checkSorted(heap: H, prev: A): Boolean = {
        println(s"checkSorted called with prev $prev and heap $heap")
        if(isEmpty(heap)) {
            println("Encountered empty heap")
            true
        }
        else {
            val curMin = findMin(heap)
            println(s"curMin is $curMin")
            (curMin >= prev) && checkSorted(deleteMin(heap), curMin)
        }
    }

    def apply() = {
        checkSorted(List(Node(0,0,List()), Node(-1478754527,1,List(Node(1783348686,0,List())))), Int.MinValue)
    }
}

QuickCheckCheck()