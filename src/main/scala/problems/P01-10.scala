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

/*
 * Find the number of elements of a list.
 * Example:
 * scala> length(List(1, 1, 2, 3, 5, 8))
 * res0: Int = 6
 *
 */

object P04 {
  def listLength[T](l:List[T]):Int = {

    l match {
      case Nil => 0
      case _ :: rest => 1 + listLength(rest)
    }
  }
}

/*
 * Example:
 * scala> reverse(List(1, 1, 2, 3, 5, 8))
 * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
 */

object P05 {
  def reverseList[T](l:List[T]):List[T] = {

    def revHelper(acc: List[T], remaining:List[T]):List[T] = {
      remaining match {
        case Nil => acc
        case h :: rest => revHelper(h +: acc, rest) 
      }
    }

    revHelper(List[T](), l)
  }
}
