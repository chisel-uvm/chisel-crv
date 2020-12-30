package chisel.bundle.random

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.tester.{testableClock, testableData, ChiselScalatestTester}
import chisel3.util._
import crv.backends.jacop.backendsjacop.RandomBundleWrapper
import crv.backends.jacop.experimental.RandBundle
import org.scalatest.{FlatSpec, Matchers}

class AluInput(val size: Int) extends Bundle {
  val a = UInt(size.W)
  val b = UInt(size.W)
  val fn = UInt(2.W)

  // Ugly
  override def typeEquivalent(data: Data): Boolean = {
    data match {
      case _: (AluInput @AluInputConstraint(size)) => true
      case _ => false
    }
  }
}

class AluInputConstraint(size: Int) extends AluInput(size) with RandBundle {

  override def typeEquivalent(data: Data): Boolean = {
    data match {
      case _: (AluInput @AluInputConstraint(size)) => true
      case _ => false
    }
  }

  def expectedResult(): AluOutput = {
    var result: BigInt = 0
    if (fn.litValue() == 0) {
      result = a.litValue() + b.litValue()
    } else if (fn.litValue() == 1) {
      result = a.litValue() - b.litValue()
    } else if (fn.litValue() == 2) {
      result = a.litValue() | b.litValue()
    } else {
      result = a.litValue() & b.litValue()
    }
    new AluOutput(8).Lit(_.result -> result.U)
  }

  // Constraints
  (a #+ b) #<= 255
  (a #- b) #>= 0
  fn #<= 3
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
class AluRandomTest extends FlatSpec with ChiselScalatestTester with Matchers {

  it should "Test the ALU with random Transactions in form of bundle" in {
    test(new Alu(8)) { alu =>
      val transaction = new AluInputConstraint(8)
      for (i <- Range(0, 10)) {
        val currentT = transaction.randomBundle()
        alu.input.poke(currentT)
        alu.clock.step()
        alu.output.expect(currentT.expectedResult())
      }
    }
  }
}
