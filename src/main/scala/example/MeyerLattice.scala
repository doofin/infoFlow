package example

import infoFlowAST.InfoFlowStmt._
import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._

import com.doofin.stdScala._
import types._

// Meyer's
object MeyerLattice extends lattice[MeyerLattice] {

  def owners(s: MeyerLattice) = s.map(_.owner)
  def readers(s: MeyerLattice, o: String) = s.find(_.owner == o).map(_.reader).getOrElse(Set())

  override val bottom = Set()

  // ⊑
  override def <=(a: MeyerLattice, b: MeyerLattice): Boolean = {
    (owners(a) subsetOf owners(b)) && (owners(a) forall (o => readers(b, o) subsetOf readers(a, o)))
  }

  override def lub(a: MeyerLattice, b: MeyerLattice): MeyerLattice = {
    val newo = owners(a) union owners(b)
    val r = newo map (o => Rules(o, readers(a, o) union readers(b, o)))
    r
  }

  override def glb(a: MeyerLattice, b: MeyerLattice): MeyerLattice = {
    val newo = owners(a) intersect owners(b)
    val r = newo map (o => Rules(o, readers(a, o) intersect readers(b, o)))
    r
  }

}
