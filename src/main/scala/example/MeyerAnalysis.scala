package example

import infoFlowAST._
import infoFlowAST.InfoFlowStmt._

import com.doofin.stdScala._
import MeyerLattice._
import types._

import scala.io.AnsiColor._
// Meyer's
object MeyerAnalysis {
  // returns (security class,is constraints satisified)
  def checkMeyer(
      st: InfoFlowStmt,
      initLev: MeyerLatticeMap,
      actAs: Map[String, String]
  ): (MeyerLattice, Boolean) = {
    def checkStmt(
        st: InfoFlowStmt,
        initLev: MeyerLatticeMap
    ): (MeyerLattice, Boolean) = { //(RuleLattice, isOK)
      def checkExpr(e: Expr): MeyerLattice = {
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

      val r: (MeyerLattice, Boolean) = st match {
        case IfActsFor(process, asAuthority, assign(v, Declassify(expr, rules))) =>
          val actAsOk =
            actAs.get(process) match {
              case None        => false
              case Some(value) => value == asAuthority
            }
          if (!actAsOk) println(process + "can't act as " + asAuthority + " !")
          val ownerRemoved = checkExpr(expr).filterNot(_.owner == asAuthority)
          val rulesOrder = <=(ownerRemoved, rules) && <=(rules, initLev(v))
          if (!rulesOrder) println("rulesOrder violated:", ownerRemoved, rules, initLev(v))
          (checkExpr(v), rulesOrder && actAsOk)
        case DeclassifyAssign(v, asAuthority, expr) =>
          (checkExpr(v), <=(checkExpr(expr).filterNot(_.owner == asAuthority), initLev(v)))
        case Stmts(xs) =>
          val resList = xs.map(checkStmt(_, initLev))
          (resList.map(_._1).reduce(glb), resList.forall(_._2))
        case assign(nm, e) =>
          /* if (nm.x contains ("test")) {
            println("assign:")
            ppc(
              (
                assign(nm, e),
                checkExpr(nm),
                (checkExpr(e), initLev(nm)),
                <=(checkExpr(e), initLev(nm))
              )
            )

          } */
          (checkExpr(nm), <=(checkExpr(e), initLev(nm)))
        case InfoFlowStmt.If_(b, s1, s2) =>
          val s1r = checkStmt(s1, initLev)._1
          val s2r = checkStmt(s2, initLev)._1

          // todo : ad hoc method to deal with EmptyStmt which should have no effect!
          val lowerB =
            (s1 == EmptyStmt, s2 == EmptyStmt) match {
              case (true, _) => s2r
              case (_, true) => s1r
              case _         => glb(s1r, s2r)
            }

          /*           println("If_:")
          ppc(
            (
              InfoFlowStmt.If_(b, s1, s2),
              checkExpr(b),
              lowerB,
              <=(checkExpr(b), lowerB)
            )
          )

           */
          (lowerB, <=(checkExpr(b), lowerB))
        case While_(b, s1) =>
          val s1r = checkStmt(s1, initLev)._1
          (s1r, <=(checkExpr(b), s1r))
        case EmptyStmt => (bottom, true) // should have no effect!
        case _         => ???
      }

      if (!r._2) {

        st match {
          case Stmts(xs)    =>
          case While_(b, s) =>
          case x =>
            println("constraint failed at below ! : ")
            ppc(x)
        }
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
