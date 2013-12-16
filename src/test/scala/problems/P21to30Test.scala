package problems

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.Matchers._

class P21to30Test extends WordSpec with Matchers {
  
  "insertAt" should {
    import P21._
    
    val l = List(1,2,3,4,5,6,7,8,9,10)
    
    "work when n = 0" in {
      insertAt(0, 0, l) shouldEqual List(0,1,2,3,4,5,6,7,8,9,10)
    }
    
    "work when n = list.length" in {
      insertAt(11,10,l) shouldEqual List(1,2,3,4,5,6,7,8,9,10,11)
    }
    
    "work in general case 1" in {
      insertAt(42,5,l) shouldEqual List(1,2,3,4,5,42,6,7,8,9,10)
    }
  }
  
  "range" should {
    import P22._
    
    "work in general case 1" in {
      range(4,9) shouldEqual List(4,5,6,7,8,9)
    }
    
  }

}