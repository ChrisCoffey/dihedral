package org.ccoffey.dihedral

import org.scalacheck.Prop._
import org.scalacheck.Properties

object SetLaws extends Properties("Set"){
    import BinaryTree.{BinaryTreeSet => BTS}

    property("No element may be in the empty set. Not even the empty set.") =
        forAll{i: Int =>
            !BTS.in(i, BTS.emptySet)
        }


}
