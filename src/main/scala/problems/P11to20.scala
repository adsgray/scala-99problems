package problems

  /*
   * Modified run-length encoding.
Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
Example:

scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   */
  
object P11 {
  import P10._
    
  def encodeModified(l:List[_]):List[_] = {
    encode(l) map { case pair @ (num, elem) => { if (num == 1) elem else pair } }
  }
}

/*
 *  Decode a run-length encoded list.
Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
Example:

scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
 */
object P12 {
 
  // for flattenList
  import P07._

  private def inflatePair(p:Pair[Int, _]):List[_] = {
    p match {
      case (num, elem) if (num == 1) => List(elem)
      case (num, elem) => elem +: inflatePair(Pair(num - 1, elem))
    }
  }
 
  def decode(l:List[Pair[Int, _]]): List[_] = {

    def decodeHelper(l:List[Pair[Int, _]], acc: List[_]):List[_] = {
      l match {
        case Nil => acc
        case x :: xs if (acc.isEmpty) => decodeHelper(xs, inflatePair(x))
        case x :: xs => decodeHelper(xs, acc ::: inflatePair(x))
      }
    }
    
    decodeHelper(l, List())
  }
  
  def decode2(l:List[Pair[Int, _]]): List[_] = flattenList(l map { inflatePair(_) })
  
  /*
  def decode3(l:List[Pair[Int, _]]): List[_] = {
    l.foldLeft(List()) {
      case (acc, pair) => acc :: inflatePair(pair)
    }
  }
  */
}

/*
Run-length encoding of a list (direct solution).
Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written 
(like P09's pack); do all the work directly.
Example:

scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
* 
*/

object P13 {
  
  def encodeDirect[T](l:List[T]):List[Pair[Int, T]] = {
    
    // Note: uses functions List.{head, init, last, tail}
    def encodeDirectHelper(l:List[T], acc:List[Pair[Int, T]]):List[Pair[Int, T]] = {
      // give these names rather than using _1 and _2 below:
      val (curcount, curitem) = acc.last
      l match {
        case Nil => acc
        case x :: xs if (x == curitem) => encodeDirectHelper(xs, acc.init :+ (curcount + 1 -> curitem))
        case x :: xs => encodeDirectHelper(xs, acc :+ (1 -> x))
      }
    }
    
    if (l.isEmpty)
      List()
    else
      // seed the first one in there so that encodeDirectHelper can always
      // assume that acc is non-null
      encodeDirectHelper(l.tail, List((1 -> l.head)))
  }
  
}


/*
 * Duplicate the elements of a list.
 * Example:
 * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
 */
object P14 {
  
  // apparently we're allowed to use flatMap
  def duplicate[T](l:List[T]):List[T] = {
    l flatMap { x => List(x,x)}
  }

}


/*
 * Duplicate the elements of a list a given number of times.
 * Example:
 * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
 * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
 */
object P15 {
  
  def duplicateN[T](n:Int, l:List[T]):List[T] = {
    def makeListN(n:Int, item:T):List[T] = {
      n match {
        case 0 => Nil
        case _ => item :: makeListN(n - 1, item)
      }
    }

    l flatMap { x => makeListN(n, x) }
  }
}


/*
 * Drop every Nth element from a list.
 * Example:
 * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
 */
object P16 {
}

/*
 * Split a list into two parts.
 * The length of the first part is given. Use a Tuple for your result.
 * Example:
 *
 * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 */
object P17 {
}

/*
 *  Extract a slice from a list.
 *  Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the
 *  Kth element of the original list. Start counting the elements with 0.
 *  Example:
 *
 *  scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 *  res0: List[Symbol] = List('d, 'e, 'f, 'g)
 */
object P18 {
}

/*
 * Rotate a list N places to the left.
 * Examples:
 * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
 *
 * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
 * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
 */
object P19 {
}

/*
 * Remove the Kth element from a list.
 * Return the list and the removed element in a Tuple. Elements are numbered from 0.
 * Example:
 *
 * scala> removeAt(1, List('a, 'b, 'c, 'd))
 * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
 */
object P20 {
}
