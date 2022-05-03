package example

import com.doofin.stdScala._

import infoFlowLattice._
import infoFlowAST._
import infoFlowAST.InfoFlowStmt._
import infoFlowTree._

import MeyerLattice._
import MeyerAnalysis._
import types._

object analysisDemo {
  def run = {

    // Denning's
    encloseDebug("Denning's") {
      Seq(
        testdata.testdata1,
        testdata.testdata2,
        testdata.testdata3,
        testdata.testdata4,
        book_appointment.book_appointment_data,
        mark_procedure.mark_procedure_data,
        retrieve_results.retrieve_results_data,
        get_statistics.get_statistics_data
      ).zipWithIndex foreach { case ((name, lev, stmt), i) =>
        val res = checkInfoFlow(stmt, lev)
        println(s"test case $i: " + name + ", result: " + res._2)
      }
    }

    encloseDebug("Meyer's") {
      Seq(MeyerTest1, MeyerTest2, MeyerTest3).zipWithIndex foreach {
        case ((program1, initLevel, actAsList, desc), i) =>
          val checkRes = checkMeyer(program1, initLevel, actAsList)
          val msg =
            if (checkRes._2)
              "success"
            else "failed"

          println(s"test case $i: ", desc, "result:" + msg)
      }
    }
  }

  def MeyerTest1 = {
    // allow proc2 to act as client
    val actAsList = Map("proc1" -- "client")

    // secure level for vars
    val initLevel: RuleLatticeMap = Map(
      (Var("a"), Set(Rules("client", Set("chkr")), Rules("chkr", Set("chkr")))), // high lev
      (Var("b"), Set(Rules("chkr", Set("chkr")))) // low lev
    )
    // a>=b, b := declassify a
    val program1 = stmts(
      IfActsFor(
        "proc1", // process 1
        "client", // act as client ("client" ok, "chkr" bad)
        Var("b") := Declassify(Var("a"), Set(Rules("chkr", Set("chkr"))))
      )
    )

    (program1, initLevel, actAsList, "proc2 can act as client (should be success)")

  }

  def MeyerTest2 = {
    // allow proc2 to act as client
    val actAsList = Map("proc1" -- "client")

    // secure level for vars
    val initLevel: RuleLatticeMap = Map(
      (Var("a"), Set(Rules("client", Set("chkr")), Rules("chkr", Set("chkr")))), // high lev
      (Var("b"), Set(Rules("chkr", Set("chkr")))) // low lev
    )
    // a>=b, b := declassify a
    val program1 = stmts(
      IfActsFor(
        "proc1", // process 1
        "chkr", // act as client ("client" ok, "chkr" bad)
        Var("b") := Declassify(Var("a"), Set(Rules("chkr", Set("chkr"))))
      )
    )

    (program1, initLevel, actAsList, "proc2 can't act as chkr (should be failed)")

  }

  def MeyerTest3 = {
    // allow proc2 to act as client
    val actAsList = Map("proc1" -- "chkr")

    // secure level for vars
    val initLevel: RuleLatticeMap = Map(
      (Var("a"), Set(Rules("client", Set("chkr")), Rules("chkr", Set("chkr")))), // high lev
      (Var("b"), Set(Rules("chkr", Set("chkr")))) // low lev
    )
    // a>=b, b := declassify a
    val program1 = stmts(
      IfActsFor(
        "proc1", // process 1
        "chkr", // act as client ("client" ok, "chkr" bad)
        Var("b") := Declassify(Var("a"), Set(Rules("chkr", Set("chkr"))))
      )
    )

    (program1, initLevel, actAsList, "proc2 act as chkr can't read (should be failed)")

  }
}
