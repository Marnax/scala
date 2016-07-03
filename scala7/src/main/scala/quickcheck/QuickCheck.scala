package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min a b") = forAll{
    (a: Int, b: Int) =>
      val m = findMin(insert(b,insert(a,empty)))
      if(a < b) m == a else m == b
  }

  property("insertAndRemoveToEmpty") = forAll {
    (a: Int) =>
      val h = insert(a, empty)
      deleteMin(h) == empty
  }

  property("minsFormSortedSequence") = forAll { h: H =>
      def minList(h2: H) : List[Int] = {
        if(h2 == empty) List()
        else findMin(h2) :: minList(deleteMin(h2))
      }
      val mins = minList(h)
      mins == mins.sorted
  }

  property("meldedMinIsMinOfMinimas") = forAll { (h1: H, h2: H) =>
    val meldMin = findMin(meld(h1,h2))
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    if(m1 < m2) meldMin == m1 else meldMin == m2
  }

  property("meldReconstruction") = forAll { (h1: H, h2: H) =>
    def minList(h2: H) : List[Int] = {
      if(h2 == empty) List()
      else findMin(h2) :: minList(deleteMin(h2))
    }

    val melding = meld(h1, h2)
    val minH1 = findMin(h1)
    val reconstructedMelding = meld(deleteMin(h1), insert(minH1, h2))
    val listMelding1 = minList(melding)
    val listMelding2 = minList(reconstructedMelding)
    listMelding1 == listMelding2
  }

}
