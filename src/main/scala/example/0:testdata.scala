package example

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import com.doofin.stdScala._

object testdata {
    def testdata1 = {
        val levs: InfoLevMap = Map(
            Var("a") -- true, Var("b") -- false, Var("c") -- false
        )

        val stmt = Stmts(List(
            Var("a") := Var("b"),
            If_(
                Var("a") <= Expr.intValue(11),
                assign(Var("b"), Expr.intValue(10)),
                assign(Var("a"), Expr.intValue(100))
          )
        ))
        ("testdata1: implicit flow in if block, a->b (should be false)", levs, stmt)
    }

    def testdata2 = {
        val levs: InfoLevMap = Map(
            Var("a") -- true, Var("b") -- false, Var("c") -- false
        )

        val stmt = Stmts(List(
            assign(Var("a"), Var("b")),
            assign(Var("b"), Expr.intValue(10)),
            assign(Var("c"), Expr.intValue(1000))
        ))
        ("testdata2: explicit flow for assignment a:=b (should be true)", levs, stmt)
    }

    def testdata3 = {
        val levs: InfoLevMap = Map(
            Var("a") -- false, Var("b") -- false, Var("c") -- false
        )

        val stmt = Stmts(List(
            assign(Var("a"), Var("b")),
            assign(Var("b"), Expr.intValue(10)),
            assign(Var("c"), Expr.intValue(1000))
        ))
        ("testdata3: explicit flow for assignment a:=b (should be true)", levs, stmt)
    }

    def testdata4 = {
        val levs: InfoLevMap = Map(
            Var("a") -- false, Var("b") -- true, Var("c") -- false
        )

        val stmt = Stmts(List(
            assign(Var("a"), Var("b")),
            assign(Var("b"), Expr.intValue(10)),
            assign(Var("c"), Expr.intValue(1000)),
            While_(Var("a") && Var("b"), EmptyStmt)
        ))
        ("testdata4: explicit flow for assignment a:=b (should be false)", levs, stmt)
    }
}
