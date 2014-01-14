package problems

import scala.annotation.tailrec


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

    @tailrec
    def revHelper(acc: List[T], remaining:List[T]):List[T] = {
      remaining match {
        case Nil => acc
        case h :: rest => revHelper(h +: acc, rest) 
      }
    }

    revHelper(List[T](), l)
  }
}

/*
 * Find out whether a list is a palindrome.
 * Example:
 * scala> isPalindrome(List(1, 2, 3, 2, 1))
 * res0: Boolean = true
 */

object P06 {

  import P04._

  def isPalindrome[T](l:List[T]):Boolean = {
    l match {
      case Nil => true
      case a :: Nil => true
      case a +: middleList :+ b => a == b && isPalindrome(middleList)
    }
  }

}

/*
 * Flatten a nested list structure.
 * Example:
 * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
 * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
 */

object P07 {

  // In the end I had to write this very "declaratively" to finally get it.
  // Basically enumerate each case?
  def flattenList[_](l:List[_]):List[_] = {
    l match {
      case Nil => Nil
      case (l:List[_]) :: rest => flattenList(l) ::: flattenList(rest)
      case x :: rest => x +: flattenList(rest)
    }
  }
}


/*
 * Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:

scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
 */

object P08 {

  def compress(l:List[_]):List[_] = {
    
    def compressHelper(l:List[_], acc: List[_], curelement: Any):List[_] = {
      l match {
        case Nil => acc
        case x :: xs if (x == curelement) => compressHelper(xs, acc, curelement)
        case x :: xs => compressHelper(xs, acc :+ x, x)
      }
    }
    
    compressHelper(l, List(), Nil)
  }
  
}

/*
 * Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.
Example:

scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
 * 
 */

object P09 {
  def pack(l:List[_]):List[List[_]] = {
    
    // In this case enumerating the different cases is made ugly
    // by the checks on sublistacc. Those checks are necessary
    // otherwise List() is included in the results.
    def packHelper(l:List[_], acc:List[List[_]], sublistacc:List[_], curelement: Any):List[List[_]] = {
      l match {
        case Nil if (!sublistacc.isEmpty) => acc :+ sublistacc
        case Nil => acc 
        case x :: xs if (x == curelement) => packHelper(xs, acc, sublistacc :+ x, curelement)
        case x :: xs if (!sublistacc.isEmpty )=> packHelper(xs, acc :+ sublistacc, List(x), x)
        case x :: xs => packHelper(xs, acc, List(x), x)
      }
    }
    
    packHelper(l, List(), List(), Nil)
  }
}

/*
 * Run-length encoding of a list.
Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
Example:

scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
 * 
 */

object P10 {
  import P04._
  import P09._

  def encode(l:List[_]):List[(Int,_)] = {
    // cheating by using map
    pack(l) map {
      l => Pair(listLength(l), l.head)
    }
  }
  
}