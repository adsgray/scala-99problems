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
  
  
  "penultimate" should {
    "return penultimate element of a list of 2" in {
      val l = List(1,2)
      P02.penultimate(l) shouldEqual 1
    }
    
    "return penultimate element of a list > 2" in {
      val l = List(1,2,3,4,5)
      P02.penultimate(l) shouldEqual 4
    }
    
    "throw an exception for an empty list" in {
      val l = List()
      an [Exception] should be thrownBy P02.penultimate(l)
    }
    
    "throw an exception for a list of 1?" in {
      val l = List(42)
      an [Exception] should be thrownBy P02.penultimate(l)
    }
    
  }
  
  
}