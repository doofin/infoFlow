package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object get_statistics {
    def get_statistics_data = {
        val levs: InfoLevMap = Map(
            Var("procedure_results") -- true,
            Var("procedure_types") -- true,
            Var("procedure_result") -- false,
            Var("procedure_type") -- false,
            Var("tests_count") -- false,
            Var("tests_positive") -- false,
            Var("test_positivity_percentage") -- false,
        )

        val stmt = Stmts(List(
            Var("procedure_result") := Var("procedure_results"),
            Var("procedure_type") := Var("procedure_types"),
            Var("tests_count") := Expr.intValue(0),
            Var("tests_positive") := Expr.intValue(0),
            While_(
                Var("procedure_result"),
                stmts(
                If_(
                    Var("procedure_type") === Expr.intValue(1),
                        If_(
                            Var("procedure_result") === Expr.intValue(1),
                            Var("tests_positive") := Var("tests_positive") + Expr.intValue(1),
                            EmptyStmt
                        ),
                        EmptyStmt
                ),
                Var("tests_count") := Var("tests_count") + Expr.intValue(1),
                Var("procedure_result") := Var("procedure_results"),
                Var("procedure_type") := Var("procedure_types")
                )
            ),
            Var("test_positivity_percentage") := Var("tests_positive") / Var("tests_count")
        ))
        ("get_statistics", levs, stmt)
    }
}