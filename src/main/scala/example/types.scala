package example

import infoFlowAST.InfoFlowStmt._
import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._

import com.doofin.stdScala._

object types {
  type RuleLattice = Set[Rules]
  type RuleLatticeMap = Map[Var, RuleLattice] // add bottom type mutable.Map[Var, Interval]
}
