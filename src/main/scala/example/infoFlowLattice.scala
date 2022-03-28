package example

import infoFlowAST._
//import cuttingedge.progAnalysis.ast.Expr.Var

object infoFlowLattice {

  type InfoLev = Boolean // low:false,high:true
  type InfoLevMap = Map[Var, InfoLev] // add bottom type mutable.Map[Var, Interval]

//  Low is smaller than High
  def <=(a: InfoLev, b: InfoLev): Boolean = {
    (a, b) match {
      case (_, true) | (false, false) => true
      case _                          => false
    }
  }

  // m1 is subset of m2
  def <=(m1: InfoLevMap, m2: InfoLevMap): Boolean = {
    val bool = m1 forall { k1 =>
      val i1 = k1._2
      val i2 = m2(k1._1)
      <=(i1, i2)
    }
    bool
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

  /**least upper bound operator*/
  val lubM: (InfoLevMap, InfoLevMap) => Map[Var, Boolean] = { (i1: InfoLevMap, i2: InfoLevMap) =>
    val newmap = (i1.keys ++ i2.keys).toSet map { k: Var =>
      val i1o = i1(k)
      val i2o = i2(k)
      val rr = lub(i1o, i2o)
      (k, rr)
    }
    val r = Map(newmap.toSeq: _*)
    r
  }
}
