package org.ccoffey.dihedral


case class FunctionSet[A](rule: A => Boolean) {
  def `in:`(a: A): Boolean =
    rule(a)

  def union(r: FunctionSet[A]): FunctionSet[A] =
    FunctionSet(x => rule(x) || r.rule(x))

  def intersect(r: FunctionSet[A]): FunctionSet[A] =
    FunctionSet(x => rule(x) && r.rule(x))

  def difference(r: FunctionSet[A]): FunctionSet[A] =
    FunctionSet(x => rule(x) && !r.rule(x))

  def symmetricDiffernce(r: FunctionSet[A]): FunctionSet[A] =
    FunctionSet(x => (rule(x) && !r.rule(x)) || (!rule(x) && r.rule(x)))

  // cannot support subset
}
// This can support most other set operations though, with virtually no memory overhead.
// a useful representation if you're dealing only with abstract sets & can affort to generate data elements outside of the object.
// Will result in absurdly complex functions for large inputs though

//what about a set typeclass?? Can I use these functions to implement all of the set behaviors above?
trait Set[S[_]]{
  def empty[A](s: S[A])(implicit o: Ordering[A]): Boolean
  def in[A](a: A, s: S[A])(implicit o: Ordering[A]): Boolean
  def insert[A](a: A, s: S[A])(implicit o: Ordering[A]): S[A]

    //2 additional functions for completeness
    def element[A](s: S[A]): Option[A]
    def remove[A](a: A, s: S[A])(implicit o: Ordering[A]): S[A]
}




//1 the above implementation is incomplete. You cannot implement most set operations because there is no means
// of getting an element out of the set or traversing the set

sealed trait BinaryTree[+A]
case class EmptyBinaryTree[+A]() extends BinaryTree[A]
case class BinaryTreeNode[+A](l: BinaryTree[A], value: A, r: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
    def show[A](tree: BinaryTree[A]): Unit = {
        
    }

   implicit object BinaryTreeSet extends Set[BinaryTree] {
     def empty[A](s: BinaryTree[A])(implicit o: Ordering[A]) =
      s match {
        case EmptyBinaryTree() => true
        case _ => false
      }

     def insert[A](a: A, s: BinaryTree[A])(implicit o: Ordering[A]): BinaryTree[A] =
      s match {
        case EmptyBinaryTree() => BinaryTreeNode(EmptyBinaryTree(), a, EmptyBinaryTree())
        case BinaryTreeNode(l, v, r) =>
          if(o.lt(a, v)) BinaryTreeNode(insert(a, l), v, r)
          else if (o.gt(a, v)) BinaryTreeNode(l, v, insert(a, r))
          else s
      }

     def in[A](a: A, s: BinaryTree[A])(implicit o: Ordering[A]): Boolean =
      s match {
        case EmptyBinaryTree() => false
        case BinaryTreeNode(l, v, r) =>
          if(o.lt(a, v))        in(a, l)
          else if(o.gt(a, v))   in(a, r)
          else o.equiv(a, v)
      }

     def element[A](s: BinaryTree[A]): Option[A] =
        s match {
            case EmptyBinaryTree() => None
            case BinaryTreeNode(_, v, _) => Some(v)
        }

     def remove[A](a: A, s: BinaryTree[A])(implicit o: Ordering[A]): BinaryTree[A] = {
        def removeMin(node: BinaryTree[A]): (BinaryTree[A], A) =
            node match {
                case EmptyBinaryTree() => null //not possible!
                case BinaryTreeNode(EmptyBinaryTree(), v, r) => (r, v)
                case BinaryTreeNode(l, v, r) =>
                    val (t, x) = removeMin(l)
                    (BinaryTreeNode(r, v, t), x)
            }

         s match {
             case EmptyBinaryTree() => s
             case BinaryTreeNode(l, v, r) if o.lt(a, v) => BinaryTreeNode(remove(a, l), v, r)
             case BinaryTreeNode(l, v, r) if o.gt(a, v) => BinaryTreeNode(l, v, remove(a, r))
             case BinaryTreeNode(l, v, r) if o.equiv(a, v) =>
                 (l, r) match {
                     case (EmptyBinaryTree(), _) => r
                     case (_, EmptyBinaryTree()) => l
                     case (BinaryTreeNode(_, _, _), _) =>
                         val (t, x) = removeMin(r)
                         BinaryTreeNode(l, x, t)
                 }
         }
     }
   }
}



//sealed trait Color
//case object Red extends Color
//case object Black extends Color
//
//sealed trait RedBlackTree[A]
//case object Empty extends RedBlackTree[Nothing]
//case class RedBlackTreeNode[A](c: Color, l: RedBlackTree[A], value: A, r: RedBlackTree[A]) extends RedBlackTree[A]
//object RedBlackTree {
//  def balance[A](t: RedBlackTree[A]): RedBlackTree[A] =
//    t match {
//
//    }
//
//}



