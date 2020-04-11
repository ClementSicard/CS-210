package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.{BooleanOperators => _, _}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val min = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(min, h)) == min
  }

  property("min1") = forAll { (i: Int) =>
    val h = insert(i, empty)
    findMin(h) == i
  }

  property("findMin from heap of 2 elements should yield min") = forAll { (i: Int, j : Int) =>
    val e = insert(i, empty)
    val e1 = deleteMin(e)
    e1 == empty
  }

  property("findMin from heap of 1 element should yield this particular element") = forAll { (i : Int) =>
    val h = insert(i, empty)
    findMin(h) == i  
  }


  property("insert into an empty heap then delete min and resulting tree should be empty") = forAll { (i : Int) =>
    val e1 = insert(i, empty)
    val e2 = deleteMin(e1)
    e2 == empty
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") = forAll { (h1 : H, h2 : H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))  
  }

  property("Any heap should get a sorted sequence when continually apply findMin and deleteMin") = forAll { (h : H) =>
    def helper(h : H): List[Int] = h match {
      case h if h == empty => Nil
      case h => findMin(h) :: helper(deleteMin(h))
    }
    val a = helper(h)
    
    (a, a.tail).zipped.forall(_ <= _) 
  }

  property("Insert 2 elements in empty") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    (if (a < b) a else b) == findMin(h)
  }
  
  property("Associative meld") = forAll { (h1: H, h2: H, h3: H) =>
    val m1 = meld(h1, meld(h2, h3))
    val m2 = meld(meld(h1, h2), h3)
    
    toList(m1) == toList(m2)
  }

  private def toList(h: H): List[Int] = h match {
    case h if isEmpty(h) => Nil
    case _ => findMin(h) :: toList(deleteMin(h))
  }
}
