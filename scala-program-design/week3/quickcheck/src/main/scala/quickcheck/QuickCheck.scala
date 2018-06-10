package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //   If you insert any two elements into an empty heap,
  //   finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("gen2") = forAll { (i1: Int, i2: Int) =>
    val min = i1 min i2
    findMin(insert(i2, insert(i1, empty))) == min
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("gen3") = forAll { i: Int => isEmpty(deleteMin(insert(i, empty))) }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minimal.
  property("gen4") = forAll { h: H =>
    val elements = exhaust(h, List())
    elements == elements.sorted.reverse
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("gen5") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    exhaust(h, List()) == (exhaust(h1, List()) ++ exhaust(h2, List())).sorted.reverse
  }

  private def exhaust(h: H, acc: List[Int]): List[Int] = {
    if (isEmpty(h)) acc
    else {
      val min = findMin(h)
      exhaust(deleteMin(h), min :: acc)
    }
  }
}
