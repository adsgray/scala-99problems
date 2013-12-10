package problems

/*
 * Find the last element of a list.
Example:
scala> last(List(1, 1, 2, 3, 5, 8))
res0: Int = 8
 */
object P01 {
  
  // assumes list has at least one element
  def lastElement(l:List[Int]):Int = {
    
    // be sure to spell Nil correctly...
    l match {
      case elem :: Nil => elem
      case elem :: rest => lastElement(rest)
      case _ => throw new Exception("burp")
    }
    
  }

}

/*
 * Find the last but one element of a list.
Example:
scala> penultimate(List(1, 1, 2, 3, 5, 8))
res0: Int = 5
 */
object P02 {
  
  def penultimate[T](l:List[T]):T = {
    l match {
      case pen :: last :: Nil => pen
      case h :: rest => penultimate(rest)
      case _ => throw new Exception("list not long enough?")
    }
  }
  
}


/*
P03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
* 
*/

object P03 {
  def kth[T](which: Int, l:List[T]):T = {
    
    def kthHelper(l:List[T], pos:Int):T = {
      l match {
        case h :: rest if (which == pos) => h
        case h :: rest => kthHelper(rest, pos + 1)
        case _ => throw new Exception("Ran out of rings?")
      }
    }
    
    kthHelper(l, 0)
  }
}