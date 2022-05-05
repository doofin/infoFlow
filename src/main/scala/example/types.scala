package example

import infoFlowAST.InfoFlowStmt._
import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._

import com.doofin.stdScala._

object types {
  type MeyerLattice = Set[Rules]
  type MeyerLatticeMap = Map[Var, MeyerLattice] // add bottom type mutable.Map[Var, Interval]
}
