package crv.backends.jacop

import Chisel.Module
import chisel3.{dontTouch, fromIntToLiteral, Bundle, Clock, Data, Input, RawModule}
import crv.backends.jacop.RandObj.ModuleElaboration
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.experimental.DataMirror

import scala.reflect.runtime.universe.Tree

trait VerificationContext extends crv.VerificationContext {

  implicit class VerifBundle[T <: Bundle](bundle: T) extends Bundle {
    def myRand() = {
      val bund = bundle.asInstanceOf[RandObj]
      assert(bund.randomize)

      class RandomBundleWrapper extends RawModule {
        val clock = IO(Input(Clock()))
        val b = IO(Input(bundle.cloneType))
        dontTouch(b)
      }

      val module = ModuleElaboration.elaborate(() => new RandomBundleWrapper)
      val portNames = DataMirror.fullModulePorts(module).drop(1).filter(!_._2.isInstanceOf[Bundle])
      val modelBinding = portNames.zipWithIndex.map {
        case (name, index) =>
          new Function1[Bundle, (Data, Data)] {
            def apply(t: Bundle): (Data, Data) = {
              t.getElements(index) match {
                case elem: Bundle => elem.getElements(index) -> bund.currentModel(name._1).value().U
                case _ => {
                  val p = t.getElements(index)
                  val o = bund.currentModel(name._1).value().U
                  t.getElements(index) -> bund.currentModel(name._1).value().U
                }
              }
            }
          }
      }
      val randomBundle = module.b.cloneType.Lit(modelBinding.toSeq: _*)
      randomBundle
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
