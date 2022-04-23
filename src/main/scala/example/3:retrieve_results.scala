
package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object retrieve_results {
    def retrieve_results_data = {
        val levs: InfoLevMap = Map(
            Var("procedure_results") -- false,
            Var("procedure_types") -- false,
            Var("procedure_dates") -- false,
            Var("patient_ids") -- true,
            Var("admin_procedure_result") -- true,
            Var("admin_procedure_type") -- true,
            Var("admin_procedure_date") -- true,
            Var("admin_patient_id") -- true,
            Var("patient_input") -- false,
            Var("procedure_result") -- false,
            Var("procedure_type") -- false,
            Var("procedure_date") -- true,
            Var("patient_id") -- true,
        )

        val stmt = Stmts(List(
            Var("patient_id") := Var("patient_input"),
            Var("admin_procedure_result") := Var("procedure_results"),
            Var("admin_procedure_type") := Var("procedure_types"),
            Var("admin_procedure_date") := Var("procedure_dates"),
            Var("admin_patient_id") := Var("patient_ids"),
            While_(
                Var("admin_patient_id"),
                stmts(
                If_(
                    Var("patient_id") === Var("admin_patient_id") && Var("procedure_date") === Var("admin_procedure_date"),
                    stmts(
                    Var("procedure_result") := Var("admin_procedure_result"),
                    Var("procedure_type") := Var("admin_procedure_type")
                    ),
                    EmptyStmt
                ),
                Var("admin_procedure_result") := Var("procedure_results"),
                Var("admin_procedure_type") := Var("procedure_types"),
                Var("admin_procedure_date") := Var("procedure_dates"),
                Var("admin_patient_id") := Var("patient_ids"), 
                )
            )
        ))
        ("retrieve_results", levs, stmt)
    }
}