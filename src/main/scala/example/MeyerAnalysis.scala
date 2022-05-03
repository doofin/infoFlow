package example

import infoFlowAST._
import infoFlowAST.InfoFlowStmt._

import com.doofin.stdScala._
import MeyerLattice._
import types._

// Meyer's
object MeyerAnalysis {
  // returns (security class,is constraints satisified)
  def checkMeyer(
      st: InfoFlowStmt,
      initLev: RuleLatticeMap,
      actAs: Map[String, String]
  ): (RuleLattice, Boolean) = {
    def checkStmt(
        st: InfoFlowStmt,
        initLev: RuleLatticeMap
    ): (RuleLattice, Boolean) = { //(RuleLattice, isOK)
      def checkExpr(e: Expr): RuleLattice = {
        e match {
          case x: Var => initLev(x)
          case bool: ExprBool =>
            bool match {
              case Expr.Not(x)        => checkExpr(x)
              case Expr.opB(a, b, op) => lub(checkExpr(a), checkExpr(b))
              case Expr.opR(a, b, op) => lub(checkExpr(a), checkExpr(b))
              case Expr.T | Expr.F    => bottom
              case _                  => bottom
            }
          case expr_a: Expr_a =>
            expr_a match {
              case x: Var             => initLev(x)
              case Expr.intValue(x)   => bottom
              case Expr.opA(a, b, op) => lub(checkExpr(a), checkExpr(b))
            }
          case x => ???
        }
      }

      val r: (RuleLattice, Boolean) = st match {
        case IfActsFor(process, asAuthority, assign(v, Declassify(expr, rules))) =>
          val actAsOk =
            actAs.get(process) match {
              case None        => false
              case Some(value) => value == asAuthority
            }
          if (!actAsOk) println(process + "can't act as " + asAuthority + " !")
          val ownerRemoved = checkExpr(expr).filterNot(_.owner == asAuthority)
          val ok = <=(ownerRemoved, rules) && <=(rules, initLev(v)) && actAsOk
          (checkExpr(v), ok)
        case DeclassifyAssign(v, asAuthority, expr) =>
          (checkExpr(v), <=(checkExpr(expr).filterNot(_.owner == asAuthority), initLev(v)))
        case Stmts(xs) =>
          val resList = xs.map(checkStmt(_, initLev))
          (resList.map(_._1).reduce(glb), resList.forall(_._2))
        case assign(nm, e) =>
          (checkExpr(nm), <=(checkExpr(e), initLev(nm)))
        case InfoFlowStmt.If_(b, s1, s2) =>
          val s1r = checkStmt(s1, initLev)._1
          val s2r = checkStmt(s2, initLev)._1
          val lowerB = glb(s1r, s2r)
          (lowerB, <=(checkExpr(b), lowerB))
        case While_(b, s1) =>
          val s1r = checkStmt(s1, initLev)._1
          (s1r, <=(checkExpr(b), s1r))
        case EmptyStmt => (bottom, true)
        case _         => ???
      }

      if (!r._2) {
        println("! constraint failed at: ")
        ppc(st)
      }
      r
    }

    checkStmt(st, initLev)
  }

  
}

/*
    val stmt = stmts(
      DeclassifyAssign(
        Var("b"), // lower
        asAuthority = "client", //"a" "client" ok, "chkr" not ok
        Var("a") // higher
      )
    )
       println(
      "1<2",
      <=(
        Set(Rules("chkr", Set("chkr"))),
        Set(Rules("client", Set("chkr")), Rules("chkr", Set("chkr")))
      )
    ) */
