package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object book_appointment {
    def book_appointment_data = {
        val levs: InfoLevMap = Map(
            Var("procedure_dates") -- false,
            Var("procedure_types") -- false,
            Var("patient_ids") -- true,
            Var("procedure_type") -- false,
            Var("procedure_date") -- false,
            Var("patient_id") -- true,
            Var("patient_input") -- false,
            Var("taken_time") -- false,
            Var("is_free") -- false
        )

        val stmt = Stmts(List(
            Var("procedure_date") := Var("patient_input"),
            Var("procedure_type") := Var("patient_input"),
            Var("patient_id") := Var("patient_input"),
            Var("taken_time") := Var("procedure_dates"),
            Var("is_free") := Expr.intValue(1),
            While_(
                Var("taken_time") && Var("is_free"),
                stmts(
                    Var("is_free") := Expr.intValue(0),
                    Var("taken_time") := Var("procedure_date")
                )
            ),
            If_(
                Var("is_free") <= Expr.intValue(1),
                stmts(
                    Var("procedure_dates") := Var("procedure_date"),
                    Var("procedure_types") := Var("procedure_type"),
                    Var("patient_ids") := Var("patient_id")
                ),
                EmptyStmt
            )
        ))
        ("book_appointment", levs, stmt)
    }
}
