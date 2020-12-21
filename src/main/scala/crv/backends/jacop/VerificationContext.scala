package crv.backends.jacop

import chisel3.{dontTouch, fromIntToLiteral, Bundle, Clock, Data, Input, RawModule}
import crv.backends.jacop.RandObj.{CRVException, ModuleElaboration}
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.DataMirror

trait VerificationContext extends crv.VerificationContext {

  class RawModuleWrapper[T <: Bundle](bundle: T) extends RawModule {
    val clock = IO(Input(Clock()))
    val b = IO(Input(bundle.cloneType))
    dontTouch(b)
  }

  implicit class RandomBundleWrapper[T <: Bundle](bundle: T) extends Bundle {
    def randomBundle() = {
      if (!bundle.isInstanceOf[RandBundle])
        throw CRVException("ERROR: current bundle is not an instance fo RandomBundle")

      val bund = bundle.asInstanceOf[RandBundle]

      if (!bund.randomize) throw CRVException("ERROR: Chisel-crv couldn't randomize the bundle")

      val module = ModuleElaboration.elaborate(() => new RawModuleWrapper[T](bundle))
      val portNames = DataMirror.fullModulePorts(module).drop(1).filter(!_._2.isInstanceOf[Bundle])
      val modelBinding = portNames.zipWithIndex.map {
        case (name, index) =>
          new Function1[Bundle, (Data, Data)] {
            def apply(t: Bundle): (Data, Data) = {
              t.getElements(index) -> bund.currentModel(name._1).value().U
            }
          }
      }
      module.b.cloneType.Lit(modelBinding: _*)
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
