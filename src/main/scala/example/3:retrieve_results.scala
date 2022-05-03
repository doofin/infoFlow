
package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object retrieve_results {
    def retrieve_results_data = {
        val levs: InfoLevMap = Map(
            Var("procedure_results") -- true,
            Var("procedure_types") -- true,
            Var("procedure_dates") -- false,
            Var("patient_ids") -- false,
            Var("admin_procedure_result") -- true,
            Var("admin_procedure_type") -- true,
            Var("admin_procedure_date") -- false,
            Var("admin_patient_id") -- false,
            Var("patient_input") -- false,
            Var("procedure_result") -- true,
            Var("procedure_type") -- true,
            Var("procedure_date") -- false,
            Var("patient_id") -- false,
            Var("taken_time") -- false,
        )

        val stmt = Stmts(List(
            Var("patient_id") := Var("patient_input"),
            Var("admin_procedure_result") := Var("procedure_results"),
            Var("admin_procedure_type") := Var("procedure_types"),
            Var("admin_procedure_date") := Var("procedure_dates") ,
            Var("admin_patient_id") := Var("patient_ids"),
            Var("taken_time") := Var("patient_ids"),
            While_(
                Var("taken_time"),
                stmts(
                If_(
                    Var("patient_id") === Var("admin_patient_id") && Var("procedure_date") === Var("admin_procedure_date"),
                    stmts(
                    Var("procedure_result") := Var("admin_procedure_result"),
                    Var("procedure_type") := Var("admin_procedure_type")
                    ),
                    Var("taken_time") := Var("patient_ids"),
                ),
                Var("admin_procedure_result") := Var("procedure_results"),
                Var("admin_procedure_type") := Var("procedure_types"),
                Var("admin_procedure_date") := Var("procedure_dates") ,
                Var("admin_patient_id") := Var("patient_ids")
                )
            )
        ))
        ("retrieve_results", levs, stmt)
    }
}