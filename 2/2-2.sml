signature Ordered =
sig
    type T
    val eq: T*T -> bool
    val lt: T*T -> bool
    val leq: T*T -> bool
end

structure INT : Ordered =
struct
  type T = int
  fun eq (x, y) = x = y
  fun lt (x, y) = x < y
  fun leq (x, y) = x <= y
end

functor UnbalancedSet(Element: Ordered) : Set =
struct
    type Elem = Element.T
    datatype Tree = E | T of Tree * Elem * Tree;
    type Set = Tree

    val empty = E

    fun member (x, E) = false
        | member (x, T (a, y, b)) =
            if Element.lt(x, y) then member (x, a)
            else if Element.lt(y, x) then member (x, b)
            else true

    fun insert (x, E) = T (E, x, E)
        | insert (x, s as T (a, y, b)) =
            if Element.lt(x, y) then T (insert (x, a), y, b)
            else if Element.lt(y, x) then T (a, y, insert (x, b))
            else s
end


structure tree = UnbalancedSet INT

xs = tree 1 2 3
