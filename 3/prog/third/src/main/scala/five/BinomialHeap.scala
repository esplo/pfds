package five

case class Node(r: Int, e: Int, tree: List[Node])

class BinomialHeap {
  type Heap = List[Node]

  val empty: Heap = List.empty[Node]

  def isEmpty(n: Node): Boolean = n.tree.isEmpty

  val rank: (Node) => Int = {
    case Node(r, _, _) => r
  }
  val root: (Node) => Int = {
    case Node(_, e, _) => e
  }

  val link: (Node, Node) => Node = {
    case (t1@Node(r, x1, c1), t2@Node(_, x2, c2)) =>
      if (x1 <= x2) Node(r + 1, x1, t2 :: c1)
      else Node(r + 1, x2, t1 :: c2)
  }

  val insTree: (Node, Heap) => Heap = {
    case (t, Nil) => t :: Nil
    case (t, ts@t2 :: ts2) =>
      if (rank(t) < rank(t2)) t :: ts
      else insTree(link(t, t2), ts2)
  }

  def insert(x: Int, ts: Heap) = insTree(Node(0, x, Nil), ts)

  val merge: (Heap, Heap) => List[Node] = {
    case (ts, Nil) => ts
    case (Nil, ts) => ts
    case (tree1@t1 :: ts1, tree2@t2 :: ts2) =>
      if (rank(t1) < rank(t2)) t1 :: merge(ts1, tree2)
      else if (rank(t2) < rank(t1)) t2 :: merge(tree1, ts2)
      else insTree(link(t1, t2), merge(ts1, ts2))
  }

  val removeMinTree: (Heap) => (Node, Heap) = {
    case Nil => throw new Exception
    case t :: Nil => (t, Nil)
    case t :: ts =>
      val (t1, ts1) = removeMinTree(ts)
      if (root(t) < root(t1)) (t, ts)
      else (t1, t :: ts1)
  }

  def findMin(ts: Heap) = {
    val (t, _) = removeMinTree(ts)
    root(t)
  }

  val findMin2: (Heap) => Int = {
    case Nil => throw new Exception
    case t :: Nil => root(t)
    case Node(_, e, _) :: ts =>
      val min = findMin2(ts)
      if(e < min) e else min
  }

  def deleteMin(ts: Heap) = {
    val (Node(_, x, ts1), ts2) = removeMinTree(ts)
    merge(ts1.reverse, ts2)
  }
}

//
//object Five extends App {
//  val h = new BinomialHeap()
//
//  val t1 = h.insert(5, h.insert(10, Nil))
//  println(t1)
//  val t2 = h.insert(6, h.insert(2, h.insert(8, t1)))
//  println(t2)
//
//  println(h.findMin(t2) == h.findMin2(t2), h.findMin2(t2))
//  val t3 = h.deleteMin(t2)
//  println(h.findMin(t3) == h.findMin2(t3), h.findMin2(t3))
//}