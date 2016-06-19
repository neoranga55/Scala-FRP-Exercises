package lists

/**
  * Created by neoranga on 19/06/2016.
  */
object Lists {

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( f(_) :: _ )

  mapFun(List(1,2,3), (x: Int) => 2 * x)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (x, y) => 1 + y )

  lengthFun(List(1,2,3))
}
