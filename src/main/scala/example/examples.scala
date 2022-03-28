package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

// false is low,true is high
object examples {
  //implicit flow in if block , a->b (should be false)
  def testdata1 = {
    val levs: InfoLevMap =
      Map(Var("a") -- true, Var("b") -- false, Var("c") -- false) // ok

    val stmt = Stmts(
      List(
        assign(Var("a"), Var("b")),
        If_(
          Expr.opR(Var("a"), Expr.intValue(11), "<="),
          assign(Var("b"), Expr.intValue(10)),
          assign(Var("a"), Expr.intValue(100))
        )
      )
    )
    ("implicit flow in if block , a->b (should be false)", levs, stmt)
  }

  //explicit flow for assignment a:=b ( should be true)
  def testdata2 = {
    val levs: InfoLevMap =
      Map(Var("a") -- true, Var("b") -- false, Var("c") -- false) // ok

    val stmt = Stmts(
      List(
        assign(Var("a"), Var("b")),
        assign(Var("b"), Expr.intValue(10)),
        assign(Var("c"), Expr.intValue(1000))
      )
    )
    ("explicit flow for assignment a:=b ( should be true)", levs, stmt)
  }
  //explicit flow for assignment a:=b ( should be true)
  def testdata3 = {
    val levs: InfoLevMap =
      Map(Var("a") -- false, Var("b") -- false, Var("c") -- false) // ok

    val stmt = Stmts(
      List(
        assign(Var("a"), Var("b")),
        assign(Var("b"), Expr.intValue(10)),
        assign(Var("c"), Expr.intValue(1000))
      )
    )
    ("explicit flow for assignment a:=b ( should be true)", levs, stmt)
  }
  //explicit flow for assignment a:=b ( should be false)
  def testdata4 = {
    val levs: InfoLevMap =
      Map(Var("a") -- false, Var("b") -- true, Var("c") -- false)

    val stmt = Stmts(
      List(
        assign(Var("a"), Var("b")),
        assign(Var("b"), Expr.intValue(10)),
        assign(Var("c"), Expr.intValue(1000))
      )
    )
    ("explicit flow for assignment a:=b ( should be false)", levs, stmt)
  }

  def testdata5 = {
    val levs: InfoLevMap =
      Map(
          Var("appointment_dates") -- false,
          Var("appointment_types") -- true,
          Var("patient_ids") -- true,
          Var("appointment_date") -- false,
          Var("appointment_type") -- true,
          Var("patient_id") -- true,
          Var("taken_time") -- false,
          Var("is_free") -- false,
    )

    val stmt = Stmts(
      List(
        assign(Var("taken_time"), Var("appointment_dates")),
        assign(Var("appointment_date"), Var("appointment_dates")),
        assign(Var("appointment_type"), Var("appointment_types")),
        assign(Var("patient_id"), Var("patient_ids"))
      )
    )
    ("explicit flow for assignment a:=b ( should be false)", levs, stmt)
  }

  def run = {

    Seq(testdata1, testdata2, testdata3, testdata4) foreach { case (name, lev, stmt) =>
      val res = checkInfoFlow(stmt, lev)
      println("checkInfoFlow ", name, "result:", res._2)
    }

  }

}
