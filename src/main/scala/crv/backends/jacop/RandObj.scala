package crv.backends.jacop

import Chisel.{Bool, Bundle, SInt}
import chisel3.stage.{ChiselGeneratorAnnotation, DesignAnnotation}
import chisel3.{RawModule, UInt}
import chisel3.stage.phases.Convert
import org.jacop.core.IntDomain
import org.jacop.search.{
  DepthFirstSearch,
  IndomainRandom,
  PrintOutListener,
  SelectChoicePoint,
  SimpleSelect,
  SimpleSolutionListener,
  SolutionListener
}

import scala.collection.mutable
import scala.math.pow
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{runtimeMirror, typeOf}

object RandObj {

  private val addLabelFun = new ThreadLocal[mutable.Buffer[DepthFirstSearch[_ <: org.jacop.core.Var]]]

  object ModuleElaboration {
    private val converter = new Convert

    def elaborate[M <: RawModule](gen: () => M): M = {
      val genAnno = ChiselGeneratorAnnotation(gen)
      val elaborationAnnos = genAnno.elaborate
      val dut = elaborationAnnos.collectFirst { case DesignAnnotation(d) => d }.get
      dut.asInstanceOf[M]
    }
  }

  private def dfs[A <: Rand]: DepthFirstSearch[A] = {
    val label = new DepthFirstSearch[A]
    label.setAssignSolution(true)
    label.setSolutionListener(new PrintOutListener[A]())
    label
  }

  private def satisfySearch[A <: Rand](
    select:   SelectChoicePoint[A],
    listener: SolutionListener[A],
    model:    Model
  ): Boolean = {
    model.imposeAllConstraints()
    val label = dfs[A]

    label.setAssignSolution(true)
    label.setPrintInfo(false)
    addLabel(label)
    label.setSolutionListener(listener)
    listener.searchAll(false)
    label.labeling(model, select)
  }

  private def addLabel(label: DepthFirstSearch[_ <: Rand]): Unit = {
    val b = addLabelFun.get()
    if (b != null) b += label
  }
}
trait RandBundle extends RandObj {

  private val rm = runtimeMirror(getClass.getClassLoader)
  private val im = rm.reflect(this)
  private val members = im.symbol.typeSignature.members
  private def uints: Iterable[universe.Symbol] = members.filter(_.typeSignature <:< typeOf[UInt])
  private def bools: Iterable[universe.Symbol] = members.filter(_.typeSignature <:< typeOf[Bool])
  private def sints: Iterable[universe.Symbol] = members.filter(_.typeSignature <:< typeOf[SInt])

  implicit def uIntToRand(u: UInt): Rand = {
    require(u.getWidth < 30)
    val name = "b_" + im
      .reflectField(uints.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[UInt] == u).head.asTerm)
      .symbol
      .toString
      .drop(6) // drop the string "value "
    val max = pow(2, u.getWidth)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, max.toInt))
    x.asInstanceOf[Rand]
  }

  implicit def sIntToRand(s: SInt): Rand = {
    require(s.getWidth < 30)
    val name = "b_" + im
      .reflectField(sints.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[SInt] == s).head.asTerm)
      .symbol
      .toString
      .drop(6)
    val max = pow(2, s.getWidth)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, -max.toInt, max.toInt))
    x.asInstanceOf[Rand]
  }

  implicit def boolToRand(b: Bool): Rand = {
    val name = "b_" + im
      .reflectField(bools.filter(x => im.reflectField(x.asTerm).get.asInstanceOf[Bool] == b).head.asTerm)
      .symbol
      .toString
      .drop(6)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, 1))
    x.asInstanceOf[Rand]
  }

  def uRand(s: String, u: UInt): Rand = {
    require(u.getWidth < 30)
    val name = s"b_$s"
    val max = pow(2, u.getWidth)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, max.toInt))
    x.asInstanceOf[Rand]
  }

  def sRand(s: String, u: SInt): Rand = {
    require(u.getWidth < 30)
    val name = s"b_$s"
    val max = pow(2, u.getWidth - 1)
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, -max.toInt, max.toInt))
    x.asInstanceOf[Rand]
  }

  def bRand(s: String, u: Bool): Rand = {
    val name = s"b_$s"
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, 1))
    x.asInstanceOf[Rand]
  }
}

trait RandObj extends crv.RandObj {

  implicit var currentModel: Model = new Model()
  private var nOfCalls = 0
  private val listener = new SimpleSolutionListener[Rand]
  private val domainDatabase = mutable.Map[Rand, IntDomain]()
  private var problemVariables = List[Rand]()
  private var initialize = false

  /**
    * Restore the domain of all [[Rand]] variable declared in the current [[RandObj]] to their initial values
    */
  private def resetDomains(): Unit = {
    domainDatabase.foreach { k =>
      k._1.domain.setDomain(k._2)
      k._1.domain.modelConstraintsToEvaluate = Array.fill[Int](k._1.domain.modelConstraintsToEvaluate.length)(0).toArray
    }
  }

  override def toString: String = {
    val buffer = new StringBuilder()
    for (i <- Range(0, currentModel.n)) {
      buffer ++= currentModel.vars(i).toString + ", "
    }
    buffer + currentModel.randcVars.mkString(", ")
  }

  /**
    * Print all the random variables declared inside the current [[RandObj]]
    */
  def debug(): Unit = {
    problemVariables.foreach(println)
  }

  /**
    * This method is called only the first time we randomize the current [[RandObj]]
    * This is necessary because every time we assign a solution to each of the random variables, their domains are
    * shrink
    */
  private def initializeObject(): Unit = {
    initialize = true
    problemVariables = currentModel.vars.filter(x => x.isInstanceOf[Rand]).map(_.asInstanceOf[Rand]).toList
    problemVariables.foreach(x => domainDatabase += (x -> x.domain.cloneLight()))
  }

  /**
    * Randomize the current [[RandObj]]
    *
    * @return Boolean the result of the current randomization
    */
  override def randomize: Boolean = {
    currentModel.setLevel(nOfCalls)
    nOfCalls += 1
    if (!initialize) initializeObject()
    resetDomains()
    preRandomize()
    // TODO: create a better implementation of Randc in order to add constraint to them
    currentModel.randcVars.foreach(_.next())
    val result = RandObj.satisfySearch(
      new SimpleSelect[Rand](problemVariables.toArray, null, new IndomainRandom[Rand](currentModel.seed + nOfCalls)),
      listener,
      currentModel
    )
    postRandomize()
    result
  }
}
