package problems

import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.Matchers._



class P11to20Test extends WordSpec with Matchers {
  
  "encodeModified" should {
    import P11._

    "make singletons into elements" in {
      val l = List(1,2,2,3,3,3,4,4,4,5,6,6,7)
      encodeModified(l) shouldEqual List(1, Pair(2,2), Pair(3,3), Pair(3,4), 5, Pair(2,6), 7)
    }
    
  }


  "decode" should {
    import P12._

    "decode a single element thing" in {
      val l = List(Pair(1,1))
      decode(l) shouldEqual List(1)
    }

    "decode a bigger thing" in {
      val l = List(Pair(2,6), Pair(1,5), Pair(4,8), Pair(2,9))
      decode(l) shouldEqual List(6,6,5,8,8,8,8,9,9)
    }

  }


  "decode2" should {
    import P12._

    "decode a single element thing" in {
      val l = List(Pair(1,1))
      decode2(l) shouldEqual List(1)
    }

    "decode a bigger thing" in {
      val l = List(Pair(2,6), Pair(1,5), Pair(4,8), Pair(2,9))
      decode2(l) shouldEqual List(6,6,5,8,8,8,8,9,9)
    }
  }
  
  "encodeDirect" should {
        
    import P13._

    "return correct for easy case" in {
      val l = List(1)
      val expected = List(Pair(1, 1))
      
      encodeDirect(l) shouldEqual expected
    }
    
    "return correct for harder case" in {
      val l = List(1,2,2,3,3,3,4,4,4,4)
      val expected = List((1 -> 1), (2 -> 2), (3 -> 3), (4 -> 4))

      encodeDirect(l) shouldEqual expected
    }

  }
  
  "duplicate" should {
    import P14._
    
    "work on a single element list" in {
      val l = List(42)
      duplicate(l) shouldEqual List(42,42)
    }
    
    "work on a larger list" in {
      val l = List(1,2,3,4)
      duplicate(l) shouldEqual List(1,1,2,2,3,3,4,4)
    }
  }
  
  "duplicateN" should {
    import P15._
    
    "work when n = 1" in {
      val l = List(1,2)
      duplicateN(1,l) shouldEqual List(1,2)
    }
    
    "work when n = 2" in {
      val l = List(1,2)
      duplicateN(2,l) shouldEqual List(1,1,2,2)
    }
    
    "work when n = 3" in {
      val l = List(1,2)
      duplicateN(3, l) shouldEqual List(1,1,1,2,2,2)
    }
    
    "work when n = 0 (empty list?)" in {
      val l = List(1,2)
      duplicateN(0,l) shouldEqual List()
    }
  }
  
  "drop" should {
    import P16._
    val l = List(1,2,3,4,5,6,7,8,9,10,11,12)

    "work when n = 1 (empty list?)" in {
      drop(1,l) shouldEqual List()
    }
    
    "work when n = 2" in {
      drop(2,l) shouldEqual List(1,3,5,7,9,11)
    }
    
    "work when n = 3" in {
      drop(3,l) shouldEqual List(1,2,4,5,7,8,10,11)
    }
    
    "work when n = 4" in {
      drop(4,l) shouldEqual List(1,2,3,5,6,7,9,10,11)
    }
    
    "work when n does not evenly divide l.length" in {
      val l2 = List(1,2,3,4,5)
      drop(2,l2) shouldEqual List(1,3,5)
    }
  }

}
