package example

import infoFlowAST._

object infoFlowLattice extends lattice[Boolean] {

  type InfoLev = Boolean // low:false,high:true
  type InfoLevMap = Map[Var, InfoLev] // add bottom type mutable.Map[Var, Interval]

  val bottom = false
  /* ordering operation
  Low is smaller than High */
  def <=(a: InfoLev, b: InfoLev): Boolean = {
    (a, b) match {
      case (_, true) | (false, false) => true
      case _                          => false
    }
  }

  /** least upper bound operator,the maximum,similar to set union
    * Low lub High = high
    * */
  def lub(a: InfoLev, b: InfoLev) = {
    (a, b) match {
      case (_, true) | (true, _) => true
      case _                     => false
    }
  }

  /**greatest lower bound operator
    * Low glb High = Low
    * */
  def glb(a: InfoLev, b: InfoLev) = {
    (a, b) match {
      case (_, false) | (false, _) => false
      case _                       => true
    }
  }

}
