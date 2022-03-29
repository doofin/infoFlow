package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object book_appointment {
  def book_appointment_data = {
    val levs: InfoLevMap =
      Map(
        Var("appointment_dates") -- false,
        Var("appointment_types") -- true,
        Var("patient_ids") -- true,
        Var("appointment_date") -- false,
        Var("appointment_type") -- true,
        Var("patient_id") -- true,
        Var("taken_time") -- false,
        Var("is_free") -- false
      )

    val stmt = Stmts(
      List(
        assign(Var("taken_time"), Var("appointment_dates")),
        assign(Var("appointment_date"), Var("appointment_dates")),
        assign(Var("appointment_type"), Var("appointment_types")),
        assign(Var("patient_id"), Var("patient_ids")),
        While_(
          Expr.opR(Var("a"), Expr.intValue(11), "<="),
          stmts(
            assign(Var("patient_id"), Var("patient_ids")),
            assign(Var("appointment_type"), Var("appointment_types"))
          )
        )
      )
    )
    ("book_appointment", levs, stmt)
  }
}
