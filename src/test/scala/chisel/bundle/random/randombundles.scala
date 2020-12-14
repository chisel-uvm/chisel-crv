package chisel.bundle.random

import Chisel.{fromIntToWidth, fromtIntToLiteral, Vec}
import chisel.bundle.random.TestBundles.T
import chisel3.tester.ChiselScalatestTester
import chisel3.{Bundle, Input, Module, Output, UInt, Wire}
import chisel3.util.{is, switch}
import crv.backends.jacop.{Constraint, Model, Rand, RandObj, VerificationContext}
import org.scalatest.{FlatSpec, Matchers}

object TestBundles {

  class T extends Bundle with RandObj {

    currentModel = new Model(3)
    val v = Vec(20, UInt(16.W))
    val x = UInt(8.W)
    val y = UInt(8.W)
    uRand("v_10", v(10)) #= uRand("x", x)
    val greaterThen: Constraint = uRand("x", x) #> uRand("y", y)
    val lessThen:    Constraint = uRand("x", x) #< uRand("y", y)
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

  class A extends Bundle with RandObj {
    currentModel = new Model(3)

    val x = UInt(8.W)
    val y = UInt(8.W)
    val greaterThen: Constraint = x #> y
    val lessThen:    Constraint = x #< y
    class B extends Bundle with RandObj {
      val x = UInt(8.W)
    }
    val b = new B()
    b.currentModel = currentModel
    val newC: Constraint = x #= b.x
    lessThen.disable()
  }

  it should "Randomize Nested Bundles " in {
    // This is just a POC is not currently working !!!!!
    val z = new A()
    val o = z.myRand()
    assert(o.x.litValue() > o.y.litValue())
    assert(o.x.litValue() == o.b.x.litValue())
    z.greaterThen.disable()
    z.lessThen.enable()
    val t = z.myRand()
    assert(t.x.litValue() < t.y.litValue())
    assert(t.x.litValue() == t.b.x.litValue())
  }

  ignore should "Randomize Bundles with Vectors " in {
    // This is just a POC is not currently working !!!!!
    val z = new T()
    val o = z.myRand()
    assert(o.x.litValue() > o.y.litValue())
    assert(o.x.litValue() == o.v(10).litValue())
    z.greaterThen.disable()
    z.lessThen.enable()
    val t = z.myRand()
    assert(t.x.litValue() < t.y.litValue())
    assert(t.x.litValue() == t.v(10).litValue())
  }
}
