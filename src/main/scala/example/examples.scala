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
        Var("a") := Var("b"),
        If_(
          Var("a") <= Expr.intValue(11), //Expr.opR(Var("a"), Expr.intValue(11), "<="),
          assign(Var("b"), Expr.intValue(10)),
          assign(Var("a"), Expr.intValue(100))
        )
      )
    )
    ("testdata1 implicit flow in if block , a->b (should be false)", levs, stmt)
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
    ("testdata2 explicit flow for assignment a:=b ( should be true)", levs, stmt)
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
    ("testdata3 explicit flow for assignment a:=b ( should be true)", levs, stmt)
  }
  //explicit flow for assignment a:=b ( should be false)
  def testdata4 = {
    val levs: InfoLevMap =
      Map(Var("a") -- false, Var("b") -- true, Var("c") -- false)

    val stmt = Stmts(
      List(
        assign(Var("a"), Var("b")),
        assign(Var("b"), Expr.intValue(10)),
        assign(Var("c"), Expr.intValue(1000)),
        While_(Var("a") && Var("b"), EmptyStmt)
      )
    )
    ("testdata4 explicit flow for assignment a:=b ( should be false)", levs, stmt)
  }

  def run = {

    Seq(
      testdata1,
      testdata2,
      testdata3,
      testdata4,
      book_appointment.book_appointment_data
    ) foreach { case (name, lev, stmt) =>
      val res = checkInfoFlow(stmt, lev)
      println("checkInfoFlow ", name, "result:", res._2)
    }

  }

}
