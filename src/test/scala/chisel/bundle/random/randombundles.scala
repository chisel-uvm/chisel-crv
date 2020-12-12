package chisel.bundle.random

import Chisel.{fromIntToWidth, fromtIntToLiteral}
import chisel.bundle.random.TestBundles.A
import chisel3.tester.ChiselScalatestTester
import chisel3.{Bundle, Input, Module, Output, UInt, Wire}
import chisel3.util.{is, switch}
import crv.backends.jacop.{Model, Rand, RandBundle, VerificationContext}
import org.scalatest.{FlatSpec, Matchers}

object TestBundles {

  class A extends Bundle with RandBundle {
    val model = new Model(3)
    currentModel = model

    val x = UInt(8.W)
    val y = UInt(8.W)
    val c = URand("x", x) #= URand("y", y)
    val t = URand("x", x) #\= URand("y", y)
    c.disable()

    override def preRandomize(): Unit = {
      super.preRandomize()
      currentModel.vars.filter(_ != null).foreach(x => println(x))
    }
    override def postRandomize(): Unit = {
      super.postRandomize()
      currentModel.vars.filter(_ != null).foreach(x => println(x))
    }
  }

  class myPacket {
    val bundle = new A()
  }

  case class K() extends Bundle {
    val x = UInt(8.W)
    val y = UInt(8.W)
    val b = UInt(1.W)
  }
}
class Alu(size: Int) extends Module with RandBundle {

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
  behavior.of("ALU")

  it should "test static circuits" in {
    val z = new A()
    println(z.currentModel.constr)
    val o = z.myRand
    assert(o.x.litValue() != o.y.litValue())
    z.c.enable()
    z.t.disable()
    println(z.currentModel.constr)
    val k = z.myRand
    assert(k.x.litValue() == k.y.litValue())
    z.c.disable()
    z.t.enable()
    println(z.currentModel.constr)
    val t = z.myRand
    assert(t.x.litValue() != t.y.litValue())
  }
}
