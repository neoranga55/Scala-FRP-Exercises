
object pairs {
  def isPrime(n: Int) = (2 until n).forall( n % _ != 0)

  val n = 7

  (1 until n).flatMap( i =>
    (1 until i).map( j => (i, j))
  ).filter{ case (i, j) => isPrime(i + j) }
//  ).filter(tuple => tuple match {case (i, j) => isPrime(i + j)})
//  ).filter((tuple) => isPrime(tuple._1 + tuple._2))
}