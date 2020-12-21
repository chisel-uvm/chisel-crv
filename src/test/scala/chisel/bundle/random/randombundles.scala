package chisel.bundle.random

import Chisel.{fromIntToWidth, fromtIntToLiteral, Vec}
import chisel.bundle.random.TestBundles.T
import chisel3.tester.ChiselScalatestTester
import chisel3.{Bundle, Input, Module, Output, UInt, Wire}
import chisel3.util.{is, switch}
import crv.backends.jacop.{Constraint, IfThen, Model, Rand, RandBundle, RandObj, VerificationContext}
import org.scalatest.{FlatSpec, Matchers}

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
class Alu(size: Int) extends Module with RandObj {

  val io = IO(new Bundle {
    val fn:     UInt = Input(UInt(2.W))
    val a:      UInt = Input(UInt(size.W))
    val b:      UInt = Input(UInt(size.W))
    val result: UInt = Output(UInt(size.W))
  })

  val result: UInt = Wire(UInt(size.W))
  result := 0.U

  switch(io.fn) {
    is(0.U) { result := io.a + io.b }
    is(1.U) { result := io.a - io.b }
    is(2.U) { result := io.a | io.b }
    is(3.U) { result := io.a & io.b }
  }
  io.result := result
}

class AluTest extends FlatSpec with ChiselScalatestTester with VerificationContext with Matchers {
  behavior.of("Random Bundles")

  class JustBundle extends Bundle {
    val x = UInt(8.W)
    val y = UInt(8.W)
  }

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
    IfThen(x #= 8) {
      y #= 9
    }
    val c = x #= 8
    val o = y #\= 9
    o.disable()
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

  it should "Randomize Nested Bundles " in {
    // This is just a POC is not currently working !!!!!
    val z = new A()
    val o = z.randomBundle()
    assert(o.x.litValue() > o.y.litValue())
    z.greaterThen.disable()
    z.lessThen.enable()
    val t = z.randomBundle()
    assert(t.x.litValue() < t.y.litValue())
  }

  ignore should "Randomize Bundles with Vectors " in {
    // This is just a POC is not currently working !!!!!
    val z = new T()
    val o = z.randomBundle()
    assert(o.x.litValue() > o.y.litValue())
    z.greaterThen.disable()
    z.lessThen.enable()
    val t = z.randomBundle()
    assert(t.x.litValue() < t.y.litValue())
  }
}
