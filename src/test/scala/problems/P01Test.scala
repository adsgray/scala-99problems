package problems

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.Matchers._


class P01Test extends WordSpec with Matchers {
  
  "lastElement" should {

    "return last element" in {
      val l = List(1,2,3,4,5)
      P01.lastElement(l) shouldEqual 5
    }
    
    "throw an exception" in {
      val l = List()
      
      an [Exception] should be thrownBy P01.lastElement(l)
    }
  }
  
  
  
}