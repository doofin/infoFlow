package example

import infoFlowAST._
import scala.xml.dtd.Decl

object infoFlowAST {

  sealed trait InfoFlowStmt
  sealed trait Expr
  sealed trait ExprBool extends Expr

  sealed trait Expr_a extends Expr

  case class Var(x: String) extends Expr with ExprBool with Expr_a

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
    case class While_(b: ExprBool, s: InfoFlowStmt) extends InfoFlowStmt
    case object EmptyStmt extends InfoFlowStmt

    /**
     * act-as: We define that a program can run on behalf of a particular entity or role.
      • The special construct if acts for(X,Y) then Z
     checks if the current process X is allowed to assume authority Y
     and if so, executes command Z with that authority;
      */
    case class ifActsFor(s1: InfoFlowStmt, reader: String, s2: InfoFlowStmt)
        extends InfoFlowStmt //if

    /*
  Declassify: When the program is acting as a participant p, then
        it can declassify data, but only in two ways:
         It can relax p’s constraint on data
         It can remove p’s constraint on data
     */
    case class Declassify(s1: InfoFlowStmt) extends InfoFlowStmt //if
//
    /* add permission to stmt like {client:chkr} */
    case class Annotated(s1: InfoFlowStmt, rules: Seq[Rules]) extends InfoFlowStmt //if

    /* permission lattice p10 */
    case class Rules(owner: String, reader: Seq[String])

    ifActsFor(
      EmptyStmt,
      "chkr",
      Declassify(Annotated(EmptyStmt, Seq(Rules("client", Seq("chkr")))))
    )
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

  implicit class opsExpr_a(x: Expr_a) {
    def +(b: Expr_a) = opA(x, b, "+")
    def /(b: Expr_a) = opA(x, b, "+")
  }

  implicit class opsVar(x: Var) {
    def :=(b: Expr) = assign(x, b)
  }
}
