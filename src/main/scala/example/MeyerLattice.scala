package example

import infoFlowAST.InfoFlowStmt._
import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._

import com.doofin.stdScala._
import types._

// Meyer's
object MeyerLattice extends lattice[RuleLattice] {

  def owners(s: RuleLattice) = s.map(_.owner)
  def readers(s: RuleLattice, o: String) = s.find(_.owner == o).get.reader

  override val bottom = Set()

  // ⊑
  override def <=(a: RuleLattice, b: RuleLattice): Boolean = {
    (owners(a) subsetOf owners(b)) && (owners(a) forall (o => readers(b, o) subsetOf readers(a, o)))
  }

  override def lub(a: RuleLattice, b: RuleLattice): RuleLattice = {
    val newo = owners(a) union owners(b)
    val r = newo map (o => Rules(o, readers(a, o) intersect readers(b, o)))
    r
  }

  override def glb(a: RuleLattice, b: RuleLattice): RuleLattice = {
    val newo = owners(a) intersect owners(b)
    val r = newo map (o => Rules(o, readers(a, o) union readers(b, o)))
    r
  }

}
