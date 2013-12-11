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

}
