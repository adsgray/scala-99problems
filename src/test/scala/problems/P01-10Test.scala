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
  
  "kth" should {

    import P03._

    "return kth in with a normal list" in {
      val l = List(1,2,3,4,5)
      kth(0, l) shouldEqual 1
      kth(1, l) shouldEqual 2
      kth(2, l) shouldEqual 3
    }
    
    "return 0th in a list of 1 element" in {
      val l = List(42)
      P03.kth(0, l) shouldEqual 42
    }
    
    "throw an exception when k is bigger than list" in {
      val l = List(1,2)
      an [Exception] should be thrownBy P03.kth(3, l)
    }
    
    "throw an exception on an empty list" in {
      val l = List()
      an [Exception] should be thrownBy P03.kth(0, l)
      an [Exception] should be thrownBy P03.kth(3, l)
    }
    
  }


  "length" should {
    import P04._

    "return 0 for the empty list" in {
      val l = List()
      listLength(l) shouldEqual 0
    }

    "return 1 for a single element list" in {
      val l = List(42)
      listLength(l) shouldEqual 1
    }

    "work on bigger lists" in {
      val l = List(1,2,3,4,5)
      listLength(l) shouldEqual 5
    }
  }

  "reverseList" should {
    import P05._

    "return empty list when given empty list" in {
      val l = List[Int]()
      reverseList(l) shouldEqual List[Int]()
    }

    "return the list when given a unit list" in {
      val l = List(42)
      reverseList(l) shouldEqual l
    }

    "reverse a general list" in {
      val l = List(1,2,3,4)
      reverseList(l) shouldEqual List(4,3,2,1)
    }
  }

  "isPalindrome" should {
    import P06._

    "return true for empty list" in {
      val l = List()
      isPalindrome(l) shouldEqual true
    }

    "return true for singleton list" in {
      val l = List(42)
      isPalindrome(l) shouldEqual true
    }

    "return true for doublet list" in {
      val l = List(42,42)
      isPalindrome(l) shouldEqual true
    }

    "return true for some palindrome lists" in {
      val l1 = List(1,2,3,3,2,1)
      isPalindrome(l1) shouldEqual true

      val l2 = List(1,2,3,2,1)
      isPalindrome(l2) shouldEqual true
    }

    "return false for some non-palindrome lists" in {
      val l1 = List(1,2)
      isPalindrome(l1) shouldEqual false

      val l2 = List(1,2,3,3,1)
      isPalindrome(l2) shouldEqual false
    }
  }

  "flattenList" should {
    import P07._

    "return Nil when passed Nil?" in {
      val l = Nil
      flattenList(l) shouldEqual Nil
    }

    "return the list if it's already flat" in {
      val l = List(1,2,3,4,5)
      flattenList(l) shouldEqual l
    }

    "flatten one level" in {
      val l = List(List(1,2))
      flattenList(l) shouldEqual List(1,2)
    }
    
    "flatten one level with single elements in between" in {
      val l = List(List(1,2), 3, List(4,5,6))
      flattenList(l) shouldEqual List(1,2,3,4,5,6)
    }
    
    "flatten two levels" in {
      val l = List(List(List(1,2), List(3,4)), 5, List(6,7))
      flattenList(l) shouldEqual List(1,2,3,4,5,6,7)
    }
  }
  
  
}
