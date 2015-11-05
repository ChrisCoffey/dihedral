package org.ccoffey.dihedral

import org.ccoffey.dihedral.Set._

/**
  A semigroup is a structure consisting of a set G and a binary operation on G such that:
  G.1 -> if a ∈ G, a * a ∈ G
  */
//note this is a clumsy signature, but it's very flexible and enforces the semigroup laws
// wrt a set. that being said, it's going to fall down with infinite sets
trait SemiGroup[A]{
    def *[M[_]](g: M[A])(a: A, b: A)(implicit s: Set[M], o: Ordering[A]): Option[A] =
        if (( a ∈ g ) && ( b ∈ g ))
            Some(combine(a, b))
        else None

    protected def combine(a: A, b: A): A
}

object SemiGroup {

    def power[A, M[_]](g: M[A])(a: A, n: Int)
                      (implicit s: Set[M], o: Ordering[A], ev: SemiGroup[A]): Option[A] =
        if((a ∈ g) && n > 0)
           Some((0 until n).foldLeft(a)((acc, _) => ev.combine(acc, a)))
        else None


}

//note this needs rules around it
trait Monoid[A] extends SemiGroup[A] {
    def neutral: A
}

/**
    A group is a structure consisting of a set G and a binary operation * on G such that:
    G.1 -> if a, b, c ∈ G, then a * (b * c) = (a * b) * c; i.e. the binary op is associative
    G.2 -> there is an element e ∈ G such that for each a ∈ G, e * a = a; e is the identity/ left-neutral element
    G.3 -> for each a ∈ G, there is an element b ∈ G such that b * a = e; b is the left-inverse of a with respect to e

    the operation * on G is called the group operation
  */
trait Group[A] extends Monoid[A]{
    def inverse(a: A): A


    private def infinite[M[_] <: Traversable[_]](g: M[A]): Boolean =
        g match {
            case s: Stream[A] => false
            case i: Iterator[A] => false
            case _ => false
        }
    def order[M[_] <: Traversable[_]](g: M[A]): Option[Int] =
        if(infinite(g)) None
        else Some(g.size)
}

trait SubGroup[A] extends Group[A] {

}