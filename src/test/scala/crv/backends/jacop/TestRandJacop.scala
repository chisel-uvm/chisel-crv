package crv.backends.jacop
import chisel3._
import chisel3.tester.{testableClock, testableData, ChiselScalatestTester}
import chisel3.util._
import org.scalatest.{FlatSpec, Matchers}

class TestRandJacop extends FlatSpec with VerificationContext {

  it should "be able to declare a random variable and and a constraint" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      var size = new Rand("size", min, max)
      var len = new Rand("len", min, max)
      len #>= size
      val x:       crv.Constraint = len #<= size
      val y:       crv.Constraint = len #> 4
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len #+ size)
    }

    val myPacket = new Packet
    myPacket.x.disable()
    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())

    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())

    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())

    myPacket.y.disable()
    myPacket.x.enable()
    myPacket.randomize
    assert(myPacket.len.value() <= myPacket.size.value())

    myPacket.y.enable()
    myPacket.x.disable()
    myPacket.randomize
    assert(myPacket.len.value() >= myPacket.size.value())

    myPacket.y.disable()
    myPacket.x.enable()
    myPacket.randomize
    assert(myPacket.len.value() <= myPacket.size.value())

    myPacket.y.enable()
    myPacket.x.disable()
    myPacket.randomize
    assert(myPacket.len.value() >= myPacket.size.value())
  }

  it should "be able to subtract two Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len #- size)
    }
    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0).value() == myPacket.len.value - myPacket.size.value())
  }

  it should "be able to add two Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len #+ size)
      payload(1) #= (len #+ 4)
    }
    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0).value() == myPacket.len.value + myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value + 4)
  }

  it should "be able to divide two Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= len.div(size)
      payload(1) #= len.div(4)
    }
    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0).value() == myPacket.len.value / myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value / 4)
  }

  it should "be able to multiply two Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len #* size)
      payload(1) #= (len #* 4)
    }
    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0).value() == myPacket.len.value * myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value * 4)
  }

  it should "be able to constraint the reminder of Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= len.mod(size)
      payload(1) #= len.mod(4)
    }
    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0).value() == myPacket.len.value % myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value % 4)
  }

  it should "be able to constraint the exponential of Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val size = new Rand("size", 2, 3)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len #^ size)
      payload(1) #= (len #^ 3)
    }
    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0).value() == math.pow(myPacket.len.value.toDouble, myPacket.size.value().toDouble).toInt)
    assert(myPacket.payload(1).value() == math.pow(myPacket.len.value.toDouble, 3).toInt)
  }

  it should "be able to constraint less or equal then  Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #<= len
      payload(1) #<= 3
    }
    val myPacket = new Packet
    myPacket.randomize
    myPacket.debug()
    assert(myPacket.payload(0) <= myPacket.len)
    assert(myPacket.payload(1) <= 3)
  }

  it should "be able to constraint less then  Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #< len
      payload(1) #< 3
    }
    val myPacket = new Packet
    assert(myPacket.randomize)
    myPacket.debug()
    assert(myPacket.payload(0) < myPacket.len)
    assert(myPacket.payload(1) < 3)
  }

  it should "be able to constraint gather or equal than of Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 10
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 10))
      payload(0) #>= len
      payload(1) #>= 3
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0) >= myPacket.len)
    assert(myPacket.payload(1) >= 3)
  }

  it should "be able to constraint gather than of Rand var" in {
    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #> len
      payload(1) #> 3
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0) > myPacket.len)
    assert(myPacket.payload(1) > 3)
  }

  it should "be able to add Constraint Groups" in {

    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", min, max))
      val cgroup: ConstraintGroup = new ConstraintGroup {
        payload(0) #> len
        payload(1) #> 98
      }

      val negc: crv.Constraint = payload(1) #< 98
      negc.disable()
    }

    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0) > myPacket.len)
    assert(myPacket.payload(1) > 98)
    myPacket.cgroup.disable()
    myPacket.negc.enable()

    assert(myPacket.randomize)
    assert(myPacket.payload(1) < 98)
  }

  it should "be possible to add Randc variables" in {

    class Packet extends RandObj(new Model) {
      val min = 1
      val max = 100
      val len = new Rand("len", min, max)
      val randc = new Randc(min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", min, max))

      payload(0) #> len
      payload(1) #> 98

      val negc: crv.Constraint = payload(1) #< 98
      negc.disable()
    }

    val myPacket = new Packet
    assert(myPacket.randomize)
    val z: BigInt = myPacket.randc.value()
    assert(myPacket.randomize)
    val x: BigInt = if (z == myPacket.max) myPacket.min else z + 1
    assert(myPacket.randc.value() == x)
  }

  it should "be able to declare nested random classes" in {
    class Packet1(model: Model) extends RandObj(model) {
      override def toString: String = "Packet1"
      val len:               Rand = new Rand(10, 100)
    }

    class Packet2(model: Model) extends RandObj(model) {
      override def toString: String = "Packet2"
      val nestedPacket = new Packet1(model)
      val size = new Rand(10, 100)
      size #= nestedPacket.len
    }

    val myPaket = new Packet2(new Model)
    assert(myPaket._model.id == myPaket.nestedPacket._model.id)
    assert(myPaket.randomize)
    assert(myPaket.size.value() == myPaket.nestedPacket.len.value())
  }

  it should "be able to declare conditional constraint" in {
    class Packet(model: Model) extends RandObj(model) {
      override def toString: String = "Packet1"
      val len:               Rand = new Rand(1, 3)
      val c:                 Rand = new Rand(1, 100)

      IfThen(len #= 1) {
        c #= 50
      }

      IfThen(len #= 2) {
        c #= 40
      }

      IfThen(len #= 3) {
        c #= 70
      }

    }
    val myPacket = new Packet(new Model)
    assert(myPacket.randomize)
    if (myPacket.len.value() == 1) {
      assert(myPacket.c.value() == 50)
    } else if (myPacket.len.value() == 2) {
      assert(myPacket.c.value() == 40)
    } else {
      assert(myPacket.c.value() == 70)
    }
  }

  it should "be able to declare ifThenElse constraint" in {
    class Packet(model: Model) extends RandObj(model) {
      override def toString: String = "Packet1"
      val len:               Rand = new Rand(1, 3)
      val c:                 Rand = new Rand(1, 100)
      IfThenElse(len #= 1)(c #= 50)(c #= 100)
    }

    val myPacket = new Packet(new Model)
    assert(myPacket.randomize)

    if (myPacket.len.value() == 1) {
      assert(myPacket.c.value() == 50)
    } else {
      assert(myPacket.c.value() == 100)
    }
  }

  it should "be possible to assign a value to a random variable" in {
    class Packet(model: Model) extends RandObj(model) {
      val len: Rand = new Rand(1, 3)
    }

    val myPacket = new Packet(new Model)
    myPacket.len.setVar(10)
    println(myPacket.toString)
  }
}
