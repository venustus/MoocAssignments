/**
 * Created by venkat on 09/06/14.
 */



object polynomial {
    class Poly(terms0: Map[Int, Double]) {
        def this(bindings: (Int, Double)*) = this(bindings.toMap)
        val terms = terms0 withDefaultValue 0.0
        def + (other: Poly) =
            new Poly((other.terms foldLeft terms)(addTerm))

        def addTerm(terms: Map[Int, Double], term: (Int, Double)) = {
            val (exp, coeff) = term
            terms + (exp -> (coeff + terms(exp)))
        }

        def adjust(term: (Int, Double)): (Int, Double) = {
            val (exp, coeff) = term
            exp -> (coeff + terms(exp))
        }

        override def toString =
            (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
    val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
    val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
    p1 + p2
}