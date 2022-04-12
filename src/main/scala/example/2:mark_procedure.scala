package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object mark_procedure {
    def mark_procedure_data = {
        val levs: InfoLevMap = Map(
            Var("procedure_results") -- false,
            Var("procedure_dates") -- false,
            Var("procedure_types") -- false,
            Var("patient_ids") -- true,
            Var("procedure_result") -- false,
            Var("procedure_type") -- false,
            Var("procedure_date") -- false,
            Var("patient_id") -- true,
            Var("doctor_input") -- false
        )

        val stmt = Stmts(List(
            Var("procedure_result") := Var("doctor_input"),
            Var("procedure_date") := Var("doctor_input"),
            Var("procedure_type") := Var("doctor_input"),
            Var("patient_id") := Var("doctor_input"),
            Var("procedure_results") := Var("procedure_result"),
            Var("procedure_dates") := Var("procedure_date"),
            Var("procedure_types") := Var("procedure_type"),
            Var("patient_ids") := Var("patient_id"),
        ))
        ("mark_procedure", levs, stmt)
    }
}
