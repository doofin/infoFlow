package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._
import MeyerLattice._
import MeyerAnalysis._
import types._

import com.doofin.stdScala._

object get_statistics_meyer {
  def statistics_meyer = {
    val actAsList = Map("proc1" -- "patient")
    val initLevel: MeyerLatticeMap = Map(
      (
        Var("procedure_result"),
        Set(Rules("patient", Set("chkr", "patient")))
      ), // high lev
      (Var("procedure_types"), Set(Rules("patient", Set("chkr", "patient")))),
      (Var("procedure_type"), Set(Rules("patient", Set("chkr", "patient")))),
      (Var("procedure_results"), Set(Rules("patient", Set("chkr", "patient")))), //database
      (Var("tests_count"), Set(Rules("patient", Set("chkr", "patient")))), // high lev
      (Var("tests_count_public"), Set()), // low lev
      (Var("tests_positive"), Set(Rules("patient", Set("chkr", "patient")))), // high lev
      (Var("tests_positive_public"), Set()),
      (Var("test_positivity_percentage"), Set())
    )

    val stmt = Stmts(
      List(
        Var("procedure_result") := Var("procedure_results"),
        Var("procedure_type") := Var("procedure_types"),
        Var("tests_count") := Expr.intValue(0),
        Var("tests_positive") := Expr.intValue(0),
        While_(
          Var("procedure_result"),
          stmts(
            If_(
              Var("procedure_type") === Expr.intValue(1) && Var("procedure_result") === Expr
                .intValue(1),
              Var("tests_positive") := Var("tests_positive") + Expr.intValue(1),
              EmptyStmt
            ),
            Var("tests_count") := Var("tests_count") + Expr.intValue(1),
            Var("procedure_result") := Var("procedure_results"),
            Var("procedure_type") := Var("procedure_types")
          )
        ),
        // Var("tests_count_public") := Var("tests_count"), // violation
        IfActsFor(
          "proc1", // process 1
          "patient", // act as client ("client" ok, "chkr" bad)
          Var("tests_count_public") := Declassify(
            Var("tests_count"),
            Set() // declassify to public
          )
        ),
        IfActsFor(
          "proc1", // process 1
          "patient", // act as client ("client" ok, "chkr" bad)
          Var("tests_positive_public") := Declassify(
            Var("tests_positive"),
            Set() // declassify to public
          )
        ),
        Var("test_positivity_percentage") := Var("tests_positive_public") / Var(
          "tests_count_public"
        )
      )
    )
    (stmt, initLevel, actAsList, "statistics_meyer")
  }
}
