package chisel.bundle.random

import chisel3._
import chisel3.tester.{testableClock, testableData, ChiselScalatestTester}
import chisel3.{Bundle, Input, Module, MultiIOModule, Output, UInt, Wire}
import chisel3.util.{is, switch}
import crv.backends.jacop.experimental.RandBundle
import crv.backends.jacop.{Constraint, IfCon, RandObj}
import org.scalatest.{FlatSpec, Matchers}
import chisel3.experimental.BundleLiterals._
import crv.backends.jacop.backendsjacop._

object TestBundles {

  class T extends Bundle with RandBundle {
    val x = UInt(8.W)
    val y = UInt(8.W)
    val greaterThen: Constraint = x #> y
    val lessThen:    Constraint = x #< y
    lessThen.disable()
  }

  case class K() extends Bundle {
    val x = UInt(8.W)
    val y = UInt(8.W)
    val b = UInt(1.W)
  }
}

class AluInput(val size: Int) extends Bundle with RandBundle {
  val a = UInt(size.W)
  val b = UInt(size.W)
  val fn = UInt(2.W)

  255.R #> (a #+ b)
  0.R #< (a #- b)

  fn #< 3.R
}

class AluOutput(val size: Int) extends Bundle {
  val result = UInt(size.W)
}

class Alu(size: Int) extends MultiIOModule {
  val input = IO(Input(new AluInput(size)))
  val output = IO(Output(new AluOutput(size)))

  val result: UInt = Wire(UInt(size.W))
  result := 0.U

  switch(input.fn) {
    is(0.U) { result := input.a + input.b }
    is(1.U) { result := input.a - input.b }
    is(2.U) { result := input.a | input.b }
    is(3.U) { result := input.a & input.b }
  }
  output.result := result
}

class RandomBunleTests extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior.of("Random Bundles")

  class A extends Bundle with RandBundle {
    val x = UInt(8.W)
    val y = UInt(8.W)
    val greaterThen: Constraint = x #> y
    val lessThen:    Constraint = x #< y
    lessThen.disable()
  }

  class F extends Bundle with RandBundle {
    val x = UInt(8.W)
    val y = UInt(8.W)
    IfCon(x #= 8.R) {
      y #= 9.R
    }
    val c = x #= 8.R
    val o = y #\= 9.R
    o.disable()
  }

  it should "Test the ALU with random Transactions in form of bundle" in {
    def check(inputT: AluInput): AluOutput = {
      var result: BigInt = 0
      if (inputT.fn.litValue() == 0) {
        result = inputT.a.litValue() + inputT.b.litValue()
      } else if (inputT.fn.litValue() == 1) {
        result = inputT.a.litValue() - inputT.b.litValue()
      } else if (inputT.fn.litValue() == 2) {
        result = inputT.a.litValue() | inputT.b.litValue()
      } else {
        result = inputT.a.litValue() & inputT.b.litValue()
      }
      new AluOutput(8).Lit(_.result -> result.U)
    }
    test(new Alu(8)) { alu =>
      val transaction = new AluInput(8)
      for (i <- Range(0, 10)) {
        val currentT = transaction.randomBundle()
        println(currentT.fn)
        println(currentT.a)
        println(currentT.b)
        alu.input.poke(currentT)
        alu.clock.step()
        alu.output.expect(check(currentT))
      }
    }
  }

  it should "Randomize with conditional constraints" in {
    val z = new F()
    val o = z.randomBundle()
    assert(o.x.litValue() == 8)
    assert(o.y.litValue() == 9)
    z.c.disable()
    z.o.enable()
    val t = z.randomBundle()
    assert(t.y.litValue() != 9)
  }

  it should "Randomize Bundles and enable disable constraints" in {
    val z = new A()
    val o = z.randomBundle()
    assert(o.x.litValue() > o.y.litValue())
    z.greaterThen.disable()
    z.lessThen.enable()
    val t = z.randomBundle()
    assert(t.x.litValue() < t.y.litValue())
  }
}
