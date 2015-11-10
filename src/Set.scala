package org.ccoffey.dihedral

import scala.collection.{Set => SSet}

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
    def emptySet[A]: S[A]
    def empty[A](s: S[A])(implicit o: Ordering[A]): Boolean
    def in[A](a: A, s: S[A])(implicit o: Ordering[A]): Boolean
    def insert[A](a: A, s: S[A])(implicit o: Ordering[A]): S[A]

    //2 additional functions for completeness
    def element[A](s: S[A]): Option[A]
    def remove[A](a: A, s: S[A])(implicit o: Ordering[A]): S[A]
}

//all of these operations can become far more efficient if there's a means of iterating over a set
object Set {
    implicit class In[A](a: A){
        def ∈[M[_]](m: M[A])(implicit s: Set[M], o: Ordering[A]): Boolean =
            s.in(a, m)
    }

    def union[A, M[_]](m: M[A], n: M[A])(implicit s: Set[M], o: Ordering[A]): M[A] = {
       s.element(m) match {
           case None => n
           case Some(a) => union(s.remove(a, m), s.insert(a, n))
       }
    }

    //note this is left associative
    def difference[A, M[_]](m: M[A], n: M[A])(implicit s: Set[M], o: Ordering[A]): M[A] = {
        def go(acc: M[A], mm: M[A]): M[A] = {
            s.element(mm) match {
                case None => acc
                case Some(a) =>
                    if(s.in(a, n)) go(acc, s.remove(a, mm))
                    else go(s.insert(a, acc), s.remove(a, mm))
            }
        }
        go(s.emptySet, m)
    }

    def intersection[A, M[_]](m: M[A], n: M[A])(implicit s: Set[M], o: Ordering[A]): M[A] = {
        def go(acc: M[A], mm: M[A]): M[A] = {
            s.element(mm) match {
                case None => acc
                case Some(a) =>
                    if(s.in(a, n)) go(s.insert(a, acc), s.remove(a, mm))
                    else go(acc, s.remove(a, mm))
            }
        }
        go(s.emptySet, m)
    }

    def symmetricDifference[A, M[_]](m: M[A], n: M[A])(implicit s: Set[M], o: Ordering[A]): M[A] =
        union(difference(m, n), difference(n, m))

    def cardinality[A, M[_]](m: M[A])(implicit s: Set[M], o: Ordering[A]): Int = {
        def go(acc: Int, mm: M[A]): Int = {
            s.element(mm).fold(acc){a => go(acc + 1, s.remove(a, mm))}
        }
        go(0, m)
    }
}


//1 the above implementation is incomplete. You cannot implement most set operations because there is no means
// of getting an element out of the set or traversing the set

