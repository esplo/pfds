package one

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


class LeftistHeap extends Heap {
  val rank: (Heap) => Int = {
    case E => 0
    case T(r, _, _, _) => r
  }

  def makeT(x: Int, a: Heap, b: Heap): Heap =
    if (rank(a) >= rank(b)) T(rank(b) + 1, x, a, b)
    else T(rank(a) + 1, x, b, a)

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

  def insert(x: Int, h: Heap): Heap = merge(T(1, x, E, E), h)

  def insert2(x: Int, h: Heap): Heap = {
    h match {
      case E => makeT(x, E, E)
      case h@T(r, e, left, right) => {
        if (x <= e) {
          makeT(x, h, E)
        }
        else {
          makeT(e, insert2(x, left), right)
        }
      }
    }
  }

  val findMin: (Heap) => Int = {
    case E => throw new RuntimeException
    case T(_, x, _, _) => x
  }
  val deleteMin: (Heap) => Heap = {
    case E => throw new RuntimeException
    case T(_, _, a, b) => merge(a, b)
  }

  /*
    隣り合う要素を処理するのworkは ceil(logn)回呼び出し
    mergeにおける比較は、ツリーの最小値に依存
    あるツリーの最小値より小さい要素に対してしか比較を行わず、
    これは最悪時にn回になる

    n + logn + loglogn + ... ~= n だと信じると、
    n+n程度の計算量になり、O(n)
   */
  def fromList(source: List[Int]): Heap = {
    def lm(l: List[Heap]): List[Heap] = l match {
      case Nil => Nil
      case (x1 :: x2 :: xs) => merge(x1, x2) :: lm(xs)
      case (x1 :: xs) => x1 :: lm(xs)
    }
    def work(s: List[Heap]): Heap = s match {
      case (x1 :: Nil) => x1
      case l => work(lm(l))
    }

    val heaps = source.map(makeT(_, E, E))
    work(heaps)
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

//object One extends App {
//  val h = new LeftistHeap()
//
//  def test(insert: (Int, Heap) => Heap): Unit = {
//    val root = T(0, 4, E, E)
//    val t2 = insert(2, root)
//    val t3 = insert(1, insert(5, t2))
//    val t4 = insert(8, insert(0, t3))
//    val t5 = h.deleteMin(t4)
//    val t6 = h.deleteMin(t5)
//    val t7 = h.deleteMin(t6)
//
//    println("==")
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
//  test(h.insert2)
//
//  val fl = h.fromList(List(4, 2, 1, 5, 8, 0))
//  println(fl)
//  val fl1 = h.deleteMin(fl)
//  println(fl1)
//  val fl2 = h.deleteMin(fl1)
//  println(fl2)
//  val fl3 = h.deleteMin(fl2)
//  println(fl3)
//}
//
