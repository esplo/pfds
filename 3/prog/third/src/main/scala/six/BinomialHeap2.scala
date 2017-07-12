package six


case class Node[T](e: T, tree: List[Node[T]])

trait HeapBase[U] {
  type T = Int
  type Elem = T
  type HeapElem = U
  type Heap = List[HeapElem]

  val empty: Heap
  def rank(n: HeapElem): Int
  def isEmpty(n: Node[T]): Boolean

  val root: (Node[T]) => T
  val link: (HeapElem, HeapElem) => HeapElem
  val insTree: (HeapElem, Heap) => Heap

  def initial(x: T): HeapElem
  def insert(x: T, ts: Heap): Heap
  val merge: (Heap, Heap) => Heap
  val removeMinTree: (Heap) => (HeapElem, Heap)
  def findMin(ts: Heap): T
  def deleteMin(ts: Heap): Heap
}

class BinomialHeap2 extends HeapBase[(Int, Node[Int])] {
  val empty: Heap = List.empty[(Int, Node[Elem])]

  def rank(n: HeapElem): Int = n._1
  def node(n: HeapElem): Node[Elem] = n._2

  def isEmpty(n: Node[Elem]): Boolean = n.tree.isEmpty
  val root: (Node[Elem]) => Elem = {
    case Node(e, _) => e
  }

  val link: (HeapElem, HeapElem) => HeapElem = {
    case ((r1, t1@Node(x1, c1)), (_, t2@Node(x2, c2))) =>
      if (x1 <= x2) (r1+1, Node(x1, t2 :: c1))
      else (r1+1, Node(x2, t1 :: c2))
  }

  val insTree: (HeapElem, Heap) => Heap = {
    case (t, Nil) => t :: Nil
    case (t, ts@t2 :: ts2) =>
      if (rank(t) < rank(t2)) t :: ts
      else insTree(link(t, t2), ts2)
  }

  def initial(x: Elem): HeapElem = (0, Node[Elem](x, Nil))
  def insert(x: Elem, ts: Heap): Heap = insTree(initial(x), ts)

  val merge: (Heap, Heap) => Heap = {
    case (ts, Nil) => ts
    case (Nil, ts) => ts
    case (tree1@t1 :: ts1, tree2@t2 :: ts2) =>
      if (rank(t1) < rank(t2)) t1 :: merge(ts1, tree2)
      else if (rank(t2) < rank(t1)) t2 :: merge(tree1, ts2)
      else insTree(link(t1, t2), merge(ts1, ts2))
  }

  val removeMinTree: (Heap) => (HeapElem, Heap) = {
    case Nil => throw new Exception
    case t :: Nil => (t, Nil)
    case t :: ts =>
      val (t1, ts1) = removeMinTree(ts)
      if (root(node(t)) < root(node(t1))) (t, ts)
      else (t1, t :: ts1)
  }

  def findMin(ts: Heap): Elem = {
    val (t, _) = removeMinTree(ts)
    root(node(t))
  }

  def deleteMin(ts: Heap): Heap = {
    val ((r, Node(_,t1)), ts2) = removeMinTree(ts)
    t1 match {
      case Nil => ts2
      case _ =>
        val rt1: List[Node[Elem]] = t1.reverse
        val heList: List[HeapElem] = rt1.indices.toList.zip(rt1)
        merge(heList, ts2)
    }
  }
}


class ExplicitMin[T](heap: HeapBase[T]) {
  private type Elem = heap.Elem
  private type HeapElem = heap.HeapElem
  // bug?: private type Heap = heap.Heap
  private type Heap = List[HeapElem]

  sealed trait MinHeap
  case object E extends MinHeap
  case class NE(e: Elem, h: Heap) extends MinHeap

  val findMin: (MinHeap) => Elem = {
    case E => throw new Exception
    case NE(e,_) => e
  }

  def insert(x: Elem, ts: MinHeap): MinHeap = {
    ts match {
      case E => NE(x, heap.insert(x, Nil))
      case NE(e, h) =>
        val u = if(x < e) x else e
        NE(u, heap.insert(x, h))
    }
  }

  val deleteMin: (MinHeap) => MinHeap = {
    case E => throw new Exception
    case NE(e, h) =>
      val rest = heap.deleteMin(h)
      val min = heap.findMin(rest)
      NE(min, rest)
  }

  val merge: (MinHeap, MinHeap) => MinHeap = {
    case (E, h) => h
    case (h, E) => h
    case (NE(e1, h1), NE(e2, h2)) =>
      NE(Math.min(e1, e2), heap.merge(h1, h2))
  }
}


object Five extends App {
  val h = new BinomialHeap2()

  val t1 = h.insert(5, h.insert(10, Nil))
  println(t1)
  val t2 = h.insert(6, h.insert(2, h.insert(8, t1)))
  println(t2)
  /*
      List(
        (0,
          Node(6,List())
        ),
        (2,
          Node(2, List(
            Node(5, List(
              Node(10,List())
            )),
            Node(8,List())
          ))
        )
      )
   */
  println(h.findMin(t2))
  val t3 = h.deleteMin(t2)
  println(h.findMin(t3))

  println("=========")

  val h2 = new ExplicitMin[(Int, Node[Int])](h)
  val ht1 = h2.insert(5, h2.insert(10, h2.E))
  println(ht1)
  val ht2 = h2.insert(6, h2.insert(2, h2.insert(8, ht1)))
  println(ht2)
  println(h2.findMin(ht2))
  val ht3 = h2.deleteMin(ht2)
  println(h2.findMin(ht3))
}