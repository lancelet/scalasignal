package signal

import collection.generic.CanBuildFrom
import collection.immutable.{ IndexedSeq, Iterable, Seq }
import collection.mutable.ArrayBuilder

object Filter {

  def filter[T, That](b: Iterable[T], a: Iterable[T], x: Iterable[T], si: Option[Iterable[T]] = None)
  (implicit n: Fractional[T], bf: CanBuildFrom[Iterable[T], T, That], m: ClassManifest[T]): That = {

    val filterIterator = new Iterator[T] {
      // normalize a and b; dividing both by a(0)
      private val a0: T = a.head
      private val aNorm: IndexedSeq[T] = (a map { n.div(_, a0) }: Iterable[T]) toIndexedSeq
      private val bNorm: IndexedSeq[T] = (b map { n.div(_, a0) }: Iterable[T]) toIndexedSeq

      // create initial state array
      private val siLength = math.max(a.size, b.size) - 1
      private val z: Array[T] = si getOrElse { List.fill(siLength)(n.zero) } toArray

      // method to update the state array (z)
      private def updateZ(z: Array[T], bTail: IndexedSeq[T], aTail: IndexedSeq[T], xm: T, ym: T) {
	for (i <- 0 until (z.size - 1)) {
	  z(i) = n.minus(n.plus(n.times(bTail(i), xm), z(i + 1)), n.times(aTail(i), ym))
	}
	z(z.size - 1) = n.minus(n.times(bTail(z.size - 1), xm), n.times(aTail(z.size - 1), ym))
      }
      
      private val b0 = bNorm.head
      private val aTail = aNorm.tail
      private val bTail = bNorm.tail

      private val xIterator = x.iterator

      override def hasNext: Boolean = xIterator.hasNext
      override def next(): T = {
	val xm: T = xIterator.next
	val ym: T = n.plus(n.times(b0, xm), z(0))
	updateZ(z, bTail, aTail, xm, ym)
	ym
      }
    }

    val builder = bf(x)
    builder ++= filterIterator
    builder.result
  }

}
