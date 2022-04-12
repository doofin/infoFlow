package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object analysis {
    def run = {
        Seq(
            testdata.testdata1,
            testdata.testdata2,
            testdata.testdata3,
            testdata.testdata4,
            book_appointment.book_appointment_data,
            mark_procedure.mark_procedure_data
        ) foreach { case (name, lev, stmt) =>
            val res = checkInfoFlow(stmt, lev)
            println("test case: " + name + ", result: " + res._2)
        }
    }
}
