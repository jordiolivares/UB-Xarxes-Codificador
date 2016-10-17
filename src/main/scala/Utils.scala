/**
  * Created by jordi on 17/10/16.
  */
object Utils {
  def chains[T](elems: List[T]): List[(T, T)] = {
    def rest(els: List[T]): List[(T, T)] = els match {
      case List(x, y) => List((x, y))
      case x :: y :: xs => (x, y) :: rest(y :: xs)
    }
    (elems.head, elems.head) :: rest(elems)
  }
}
