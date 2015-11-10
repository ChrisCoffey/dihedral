package org.ccoffey.dihedral

sealed trait BinaryTree[+A]
case class EmptyBinaryTree[+A]() extends BinaryTree[A]
case class BinaryTreeNode[+A](l: BinaryTree[A], value: A, r: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
    def show[A](tree: BinaryTree[A]): Unit = {
        var ls: Map[Int, Seq[String]] = Map.empty[Int, Seq[String]]
        def build(depth: Int, t: BinaryTree[A], onHead: Boolean): Unit = {
            t match {
                case BinaryTreeNode(l, v, r) =>
                    val c = ls.getOrElse(depth, Seq())
                    val nC = if(onHead) v.toString +: c else c :+ v.toString
                    ls += (depth -> nC)
                    build(depth + 1, l, true)
                    build(depth + 1, r, false)
                case EmptyBinaryTree() =>
                    ()
            }
        }
        build(0, tree, true)

        println(ls.toList.sortBy(_._1).map(_._2.mkString("\t")).mkString("\n"))
    }

    implicit object BinaryTreeSet extends Set[BinaryTree] {
        def emptySet[A] = EmptyBinaryTree[A]()
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


