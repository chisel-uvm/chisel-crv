package crv.backends.jacop

import chisel3.{chiselTypeOf, dontTouch, fromIntToLiteral, Bundle, Clock, Data, Input, RawModule}
import crv.backends.jacop.RandObj.CRVException
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.DataMirror
import crv.backends.jacop.experimental.RandBundle
import crv.backends.jacop.experimental.RandBundle.ModuleElaboration

package object backendsjacop {
  class RawModuleWrapper[T <: Bundle](bundle: T) extends RawModule {
    val clock = IO(Input(Clock()))
    val b = IO(Input(bundle.cloneType))
    dontTouch(b)
  }

  implicit class RandomBundleWrapper[T <: Bundle with RandBundle](bundle: T) extends Bundle {
    def randomBundle() = {

      if (!bundle.randomize) throw CRVException("ERROR: Chisel-crv couldn't randomize the bundle")

      val module = ModuleElaboration.elaborate(() => new RawModuleWrapper[T](bundle))
      val portNames = DataMirror.fullModulePorts(module).drop(1).filter(!_._2.isInstanceOf[Bundle])
      val modelBinding = portNames.zipWithIndex.map {
        case (name, index) =>
          new Function1[Bundle, (Data, Data)] {
            def apply(t: Bundle): (Data, Data) = {
              t.getElements(index) -> bundle.currentModel(name._1).value().U
            }
          }
      }
      chiselTypeOf(module.b).Lit(modelBinding: _*)
    }
  }

  implicit class IntegerConverter(i: Int)(implicit model: Model) {
    def R(): Rand = {
      new Rand(i, i)
    }
  }

  implicit class BigIntegerConverter(i: BigInt)(implicit model: Model) {
    require(i < Int.MaxValue)
    require(i > Int.MinValue)
    def R(): Rand = {
      new Rand(i.toInt, i.toInt)
    }
  }

  /**
    * Converts a Rand to BigInt.
    *
    * @param r [[Rand]] variable to be converted.
    */
  implicit def RandToBigInt(r: Rand): BigInt = r.value()
}
