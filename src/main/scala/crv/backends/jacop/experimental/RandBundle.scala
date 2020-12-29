package crv.backends.jacop.experimental

import Chisel.{Bool, SInt}
import chisel3.stage.{ChiselGeneratorAnnotation, DesignAnnotation}
import chisel3.{RawModule, UInt}
import crv.backends.jacop.{Rand, RandObj}

import scala.math.pow
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{runtimeMirror, typeOf}

object RandBundle {

  object ModuleElaboration {
    def elaborate[M <: RawModule](gen: () => M): M = {
      val genAnno = ChiselGeneratorAnnotation(gen)
      val elaborationAnnos = genAnno.elaborate
      val dut = elaborationAnnos.collectFirst { case DesignAnnotation(d) => d }.get
      dut.asInstanceOf[M]
    }
  }
}

trait RandBundle extends RandObj {

  private val rm = runtimeMirror(getClass.getClassLoader)
  private val im = rm.reflect(this)
  private val members = im.symbol.typeSignature.members
  private def uints: Iterable[universe.Symbol] = members.filter(_.typeSignature.resultType <:< typeOf[UInt])
  private def bools: Iterable[universe.Symbol] = members.filter(_.typeSignature.resultType <:< typeOf[Bool])
  private def sints: Iterable[universe.Symbol] = members.filter(_.typeSignature.resultType <:< typeOf[SInt])

  implicit def uIntToRand(u: UInt): Rand = {
    require(u.getWidth < 30)
    val filtered = uints.filter(!_.isMacro).filter(!_.name.toString.contains("do_as"))
    val name = "b_" + im
      .reflectField(filtered.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[UInt] == u).head.asTerm)
      .symbol
      .toString
      .drop(6) // drop the string "value "
    val max = pow(2, u.getWidth)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, max.toInt))
    x.asInstanceOf[Rand]
  }

  implicit def sIntToRand(s: SInt): Rand = {
    require(s.getWidth < 30)
    val filtered = sints.filter(!_.isMacro).filter(!_.name.toString.contains("do_as"))
    val name = "b_" + im
      .reflectField(filtered.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[SInt] == s).head.asTerm)
      .symbol
      .toString
      .drop(6)
    val max = pow(2, s.getWidth)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, -max.toInt, max.toInt))
    x.asInstanceOf[Rand]
  }

  implicit def boolToRand(b: Bool): Rand = {
    val filtered = bools.filter(!_.isMacro).filter(!_.name.toString.contains("do_as"))
    val name = "b_" + im
      .reflectField(filtered.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[Bool] == b).head.asTerm)
      .symbol
      .toString
      .drop(6)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, 1))
    x.asInstanceOf[Rand]
  }

  def uRand(u: UInt): Rand = {
    require(u.getWidth < 30)
    val filtered = uints.filter(!_.isMacro).filter(!_.name.toString.contains("do_as"))
    val name = "b_" + im
      .reflectField(filtered.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[UInt] == u).head.asTerm)
      .symbol
      .toString
      .drop(6)
    val max = pow(2, u.getWidth)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, max.toInt))
    x.asInstanceOf[Rand]
  }

  def sRand(u: SInt): Rand = {
    require(u.getWidth < 30)
    val filtered = sints.filter(!_.isMacro).filter(!_.name.toString.contains("do_as"))
    val name = "b_" + im
      .reflectField(filtered.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[SInt] == u).head.asTerm)
      .symbol
      .toString
      .drop(6)
    val max = pow(2, u.getWidth - 1)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, -max.toInt, max.toInt))
    x.asInstanceOf[Rand]
  }

  def bRand(u: Bool): Rand = {
    val filtered = bools.filter(!_.isMacro).filter(!_.name.toString.contains("do_as"))
    val name = "b_" + im
      .reflectField(filtered.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[Bool] == u).head.asTerm)
      .symbol
      .toString
      .drop(6)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, 1))
    x.asInstanceOf[Rand]
  }
}
