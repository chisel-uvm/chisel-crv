package crv.backends.jacop

import chisel3.{dontTouch, fromIntToLiteral, Bundle, Clock, Data, Input, RawModule}
import crv.backends.jacop.RandBundle.ModuleElaboration
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.DataMirror

trait VerificationContext extends crv.VerificationContext {

  implicit class VerifBundle[T <: Bundle](bundle: T) extends Bundle {
    def myRand() = {
      val bund = bundle.asInstanceOf[RandBundle]
      assert(bund.randomize)

      class RandomBundleWrapper extends RawModule {
        val clock = IO(Input(Clock()))
        val b = IO(Input(bundle.cloneType))
        dontTouch(b)
      }

//      val module = ModuleElaboration.elaborate(() => new RandomBundleWrapper)
//      val portNames = DataMirror.fullModulePorts(module).drop(2)
//      val modelBinding = portNames.map(_._1).zipWithIndex.map {
//        case (name, index) =>
//          new Function1[Bundle, (Data, Data)] {
//            def apply(t: Bundle): (Data, Data) = t.getElements(index) -> bund.currentModel(name).value().U
//          }
//      }
//      val randomBundle = module.b.cloneType.Lit(modelBinding.toSeq: _*)
//      randomBundle
    }
  }

  /**
    * Converts a BigInt to [[Rand]].
    *
    * @param i integer to be converted.
    */
  implicit def BigIntToRand(i: BigInt)(implicit model: Model): Rand = {
    require(i < Int.MaxValue)
    new Rand(i.toInt, i.toInt)
  }

  /**
    * Converts a Rand to BigInt.
    *
    * @param r [[Rand]] variable to be converted.
    */
  implicit def RandToBigInt(r: Rand): BigInt = r.value()
}
