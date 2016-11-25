package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // minimum from heap
  property("gen") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // insert into empty heap, return the same value
  property("findMin") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // insert 2 ints into heap, return smallest
  property("findMin2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == scala.math.min(a,b)
  }

  // insert element into empty heap, remove element, ensure heap is empty
  property("delete1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  // delete should remove the smallest element
  property("delete2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(deleteMin(h)) == scala.math.max(a, b)
  }

  // find minimum of two heaps melded together
  property("meld") = forAll { (a: H, b: H) =>
    val min = if (findMin(a) < findMin(b)) findMin(a) else findMin(b)
    findMin(meld(a,b)) == min
  }

  // meld 2 heaps incl one empty. removing an element from the resulting heap should produce empty heap
  property("meld2") = forAll { a: Int =>
    val h = meld(insert(a, empty), empty)
    isEmpty(deleteMin(h))
  }

  property("meld3") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(deleteMin(h))) == scala.math.max(c, scala.math.max(a, b))
  }

  // ensure elements are sorted after being returned from heap
  property("sorted") = forAll { a: H =>
    def isSorted(min: Int, h: H): Boolean = {
      if (isEmpty(h)) true
      else if (min > findMin(h)) false
      else isSorted(findMin(h), deleteMin(h))
    }

    isSorted(findMin(a), deleteMin(a))
  }

}
