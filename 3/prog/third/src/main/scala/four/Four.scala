package four

sealed trait Heap

case object E extends Heap

// TODO: Intに限定したくない……
case class T(r: Int, e: Int, left: Heap, right: Heap) extends Heap

// T(r: Int, e: Int, left: Tree[Int], right: Tree[Int]) extends Heap with Tree[Int] {
//
//trait Tree[A <: Ordered[A]] {
//  val rank: Int
////  def makeT(x: Int, a: Heap[A], b: Heap[A])
//}

/*
左偏ヒープの構成条件より、最短で右スパインの値は
n = 2^{m} - 1
になったとき+1される（完全二分木）。

n = 1のとき右スパインは1、
n = 3のとき右スパインは2……となるので、
高々floor(log(n+1))になる
 */


class LeftistHeap extends Heap {
  val rank: (Heap) => Int = {
    case E => 0
    case T(r, _, _, _) => r
  }

  def makeT(x: Int, a: Heap, b: Heap): Heap =
    if (rank(a) >= rank(b)) T(rank(a) + rank(b) + 1, x, a, b)
    else T(rank(a) + rank(b) + 1, x, b, a)

  def empty = E

  def isEmpty: (Heap) => Boolean = {
    case E => true
    case _ => false
  }

  val merge: (Heap, Heap) => Heap = {
    case (h, E) => h
    case (E, h) => h
    case (h1@T(_, x, a1, b1), h2@T(_, y, a2, b2)) =>
      if (x <= y) makeT(x, a1, merge(b1, h2))
      else makeT(y, a2, merge(h1, b2))
  }

  /*
    このmergeでは merged に対する評価を行わず木を構成することができる（評価する分岐などがない）
    そのため、ノードにアクセスする必要がなければ評価が行われず、遅延評価環境では高速にマージができる
    同様の理由で並列処理も可能
   */
  def merge2: (Heap, Heap) => Heap = {
    case (h, E) => h
    case (E, h) => h
    case (h1@T(_, x, a1, b1), h2@T(_, y, a2, b2)) =>
      val (e, a, b) = if (x <= y) {
        lazy val merged = merge2(b1, h2)
        if (rank(a1) < rank(b1) + rank(h2)) (x, merged, a1)
        else (x, a1, merged)
      } else {
        lazy val merged = merge2(h1, b2)
        if (rank(a2) < rank(h1) + rank(b2)) (y, merged, a2)
        else (y, a2, merged)
      }

      val mergedRank = rank(h1) + rank(h2)
      T(mergedRank, e, a, b)
  }

  def insert(x: Int, h: Heap): Heap = merge(T(1, x, E, E), h)

  val findMin: (Heap) => Int = {
    case E => throw new RuntimeException
    case T(_, x, _, _) => x
  }
  val deleteMin: (Heap) => Heap = {
    case E => throw new RuntimeException
    case T(_, _, a, b) => merge(a, b)
  }

  val left: (Heap) => Heap = {
    case E => E
    case T(_, _, l, _) => l
  }

  val right: (Heap) => Heap = {
    case E => E
    case T(_, _, _, r) => r
  }
}
//
//object One extends App {
//  val h = new LeftistHeap()
//
//  def test(insert: (Int, Heap) => Heap): Unit = {
//    val root = T(1, 4, E, E)
//    val t2 = insert(2, root)
//    val t3 = insert(1, insert(5, t2))
//    val t4 = insert(8, insert(0, t3))
//    val t5 = h.deleteMin(t4)
//    val t6 = h.deleteMin(t5)
//    val t7 = h.deleteMin(t6)
//
//    println("==")
//    println(root)
//    println(t2)
//
//    println("  ==")
//    println(t5)
//    println(t6)
//    println(h.findMin(t7))
//
//    println("= test =")
//    println(h.findMin(t4) == 0)
//    println(h.findMin(t5) == 1)
//    println(h.findMin(t6) == 2)
//    println(h.findMin(t7) == 4)
//
//    println(h.rank(h.left(t7)) >= h.rank(h.right(t7)))
//  }
//
//  test(h.insert)
////
////
////  def makeHugeTree(n : Int): Heap = {
////    (0 to n).foldLeft(E: Heap)((a, v) =>
////      h.insert(v, a)
////    )
////  }
////
////  val t1 = makeHugeTree(5000000)
////  val t2 = makeHugeTree(5000000)
////
////  import java.time.temporal.ChronoUnit
////
////  val d = Instant.now()
////  println(d)
////  val h2 = h.merge2(t1, t2)
////  val d2 = Instant.now()
////  println(d2)
////  val minutes = ChronoUnit.NANOS.between(d, d2)
////  println(minutes, "s")
//}
//
