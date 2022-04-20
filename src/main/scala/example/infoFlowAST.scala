package example

import infoFlowAST._

object infoFlowAST {

  sealed trait InfoFlowStmt
  sealed trait Expr
  sealed trait ExprBool extends Expr

  sealed trait Expr_a extends Expr

  case class Var(x: String) extends Expr with ExprBool

  def stmts(xs: InfoFlowStmt*) = InfoFlowStmt.Stmts(xs.toList)

  object InfoFlowStmt {

    case class Stmts(xs: List[InfoFlowStmt]) extends InfoFlowStmt

    case class assign(nm: Var, a: Expr) extends InfoFlowStmt //var assignment :=

    object Expr {
      case object T extends ExprBool
      case object F extends ExprBool
      case class Not(x: ExprBool) extends ExprBool

      case class intValue(x: Int) extends Expr_a
      case class opA(a: Expr_a, b: Expr_a, op: String) extends Expr_a

      case class opB(a: ExprBool, b: ExprBool, op: String) extends ExprBool
      case class opR(a: Expr, b: Expr, op: String) extends ExprBool
    }

    case class If_(b: ExprBool, s1: InfoFlowStmt, s2: InfoFlowStmt) extends InfoFlowStmt //if
//
    case class While_(b: ExprBool, s: InfoFlowStmt) extends InfoFlowStmt
    case object EmptyStmt extends InfoFlowStmt

  }

  implicit class opsBool(x: ExprBool) {
    def &&(y: ExprBool) = InfoFlowStmt.Expr.opB(x, y, "&&")
    def ||(y: ExprBool) = InfoFlowStmt.Expr.opB(x, y, "||")
    // def <=(b: Expr) = Expr.opR(Var("a"), Expr.intValue(11), "<=")
  }

  import InfoFlowStmt.Expr._
  import InfoFlowStmt._

  implicit class opsExpr(x: Expr) {
    def <=(b: Expr) = opR(x, b, "<=")
    def >=(b: Expr) = opR(x, b, ">=")
    def ===(b: Expr) = opR(x, b, "===")
  }

  implicit class opsVar(x: Var) {
    def :=(b: Expr) = assign(x, b)
  }
}
