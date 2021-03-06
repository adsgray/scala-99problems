package problems

/*
 * Insert an element at a given position into a list.
Example:
scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
 * 
 */
object P21 {
  import P17._

  def insertAt[T](t:T, n:Int, l:List[T]):List[T] = {
    val (front,back) = split(n, l)
    (front :+ t) ::: back
  }
}

/*
 * Create a list containing all integers within a given range.
Example:
scala> range(4, 9)
res0: List[Int] = List(4, 5, 6, 7, 8, 9)
 */
object P22 {
  
  // "Recursion is the goto of functional programming." - Eric Meijer
  def range(s:Int, e:Int):List[Int] = {
    if (s == e)
      s :: Nil
    else
      s :: range(s + 1, e)
  }
}

/*
 * Extract a given number of randomly selected elements from a list.
Example:
scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
res0: List[Symbol] = List('e, 'd, 'a)
 */
object P23 {
  // for removeAt
  import P20._
  import scala.util.Random

  def randomSelect[T](n:Int, l:List[T]):List[T] = {
    n match {
      case 0 => Nil
      case n @ _ => {
        val pos = Random.nextInt(l.length)
        l(pos) :: randomSelect(n - 1, removeAt(pos, l))
      }
    }
  }
}


/*
 *  Lotto: Draw N different random numbers from the set 1..M.
Example:
scala> lotto(6, 49)
res0: List[Int] = List(23, 1, 17, 33, 21, 37)
 * 
 */
object P24 {
  import P22._
  import P23._
  
  def lotto(n:Int, m:Int):List[Int] = {
    val r = range(1,m)
    randomSelect(n, r)
  }
}

/*
 *Generate a random permutation of the elements of a list.
Hint: Use the solution of problem P23.
Example:

scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f) 
 */
object P25 {
  import P23._
  
  def randomPermute[T](l:List[T]):List[T] = randomSelect(l.length, l)

  // Note from solutions:
  // Efficient purely functional algorithms for shuffling are a lot harder.  One
  // is described in http://okmij.org/ftp/Haskell/perfect-shuffle.txt using
  // Haskell. Implementing it in Scala is left as an exercise for the reader.
}

/*
 * Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) 
denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
Example:

scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
 *   
 */
object P26 {
  import P20._ // removeAt
  import P22._ // range

  def combinations[T](n:Int, l:List[T]):List[List[T]] = {
    
    if (l.isEmpty)
      Nil
    else
      range(0, l.length - 1).foldLeft(List.empty[List[T]]) {
        case (acc, idx) => 
          for {
            // for each partial list we've constructed so far
            elem <- acc
            restcombos <- combinations(n - 1, removeAt(idx, l))
            // add the remaining to it?
          } yield elem ::: restcombos
      }
  }
}

/*
 * 
 * Group the elements of a set into disjoint subsets.
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
Example:

scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

Example:

scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, 
we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
 */
object P27 {
}

/*
 *  Sorting a list of lists according to length of sublists.
a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. 
E.g. short lists first, longer lists later, or vice versa.
Example:

scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their 
length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.

Example:

scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have 
length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
 * 
 */
object P28 {
}