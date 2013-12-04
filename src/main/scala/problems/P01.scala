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