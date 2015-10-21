package org.ccoffey.dihedral

case class Set[A](rule: A => Boolean) {
  def in:(a: A): Boolean =
    rule(a)

  def union(r: Set[A]): Set[A] = 
    Set(x => rule(x) || r.rule(x))

  def intersect(r: Set[A]): Set[A] = 
    Set(x => rule(x) && r.rule(x))

  def difference(r: Set[A]): Set[A] = 
    Set(x => rule(x) && !r.rule(x))

  def symmetricDiffernce(r: Set[A]): Set[A] = 
    Set(x => (rule(x) && !r.rule(x)) || (!rule(x) && r.rule(x)))

}

object Set {
  val emptySet = Set[Nothing](x => false)
  val naturalNumbers = Set((i: Long) => i > 0)
  val integers = Set((i: Long) => true)
  val reals = Set((i:BigDecimal) => true)
  val evensNaturals = Set((i: Long) => i in: naturalNumbers && i % 2 == 0)
}
