package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._

import com.doofin.stdScala._

object infoFlowTree {

  // returns (security class,is constraints satisified)
  def checkInfoFlow(
      st: InfoFlowStmt,
      initLev: InfoLevMap,
      leaked: Boolean = false
  ): (InfoLev, Boolean) = {

    def checkExpr(e: Expr): InfoLev = {
      e match {
        case x: Var => initLev(x)
        case bool: ExprBool =>
          bool match {
            case Expr.Not(x)        => checkExpr(x)
            case Expr.opB(a, b, op) => lub(checkExpr(a), checkExpr(b))
            case Expr.opR(a, b, op) => lub(checkExpr(a), checkExpr(b))
            case Expr.T | Expr.F    => false
          }
        case expr_a: Expr_a =>
          expr_a match {
            case Expr.intValue(x)   => false
            case Expr.opA(a, b, op) => lub(checkExpr(a), checkExpr(b))

          }
      }
    }

    val r: (InfoLev, Boolean) = st match {
      case InfoFlowStmt.Stmts(xs) =>
        val resList = xs.map(checkInfoFlow(_, initLev))
        (resList.map(_._1).reduce(glb), resList.forall(_._2))
      case x @ InfoFlowStmt.assign(nm, e) =>
        // println(x, checkExpr(e), initLev(nm))
        // (assign(Var(a),Var(b)),true,false)
        (checkExpr(nm), <=(checkExpr(e), initLev(nm)))
      case InfoFlowStmt.If_(b, s1, s2) =>
        val s1r = checkInfoFlow(s1, initLev)._1
        val s2r = checkInfoFlow(s2, initLev)._1
        val lowerB = glb(s1r, s2r)
        // println(s1r, s2r, lowerB, checkExpr(b))
        (
          lowerB,
          <=(
            checkExpr(b),
            lowerB
          )
        )
      case While_(b, s1) =>
        val s1r = checkInfoFlow(s1, initLev)._1
        (
          s1r,
          <=(
            checkExpr(b),
            s1r
          )
        )
    }

    if (!r._2) {
      println("! constraint failed at : ", st)
      // pp(st)
    }
    r
  }
}
