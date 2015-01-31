/**
 * Created by venkat on 09/06/14.
 */


object nqueens {
    def queens(n: Int): Set[List[Int]] = {
        def isSafe(col: Int, queens: List[Int]): Boolean = {
            val row = queens.length
            (queens.reverse zipWithIndex) forall (
                    {
                        case (pos, ind) => col != pos && (math.abs(col - pos) != math.abs(row - ind))
                    }
            )
        }

        def placeQueens(k: Int): Set[List[Int]] =
            if(k == 0) Set(List())
            else {
                for{
                    queens <- placeQueens(k - 1)
                    col <- 0 until n
                    if isSafe(col, queens)
                } yield col :: queens
            }
        placeQueens(n)
    }

    queens(4)
}