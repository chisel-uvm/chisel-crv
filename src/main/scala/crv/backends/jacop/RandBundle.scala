package crv.backends.jacop

import chisel3.stage.{ChiselGeneratorAnnotation, DesignAnnotation}
import chisel3.{Bool, RawModule, UInt}
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

object RandBundle {

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

trait RandBundle extends crv.RandObj {

  def URand(s: String, u: UInt): Rand = {
    val name = s"b_$s"
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, u.getWidth))
    x.asInstanceOf[Rand]
  }

  def URand(s: String, u: Bool): Rand = {
    val name = s"b_$s"
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, 0, 1))
    x.asInstanceOf[Rand]
  }

  def SRand(s: String, u: Bool): Rand = {
    val name = s"b_$s"
    val x = currentModel.vars.filter(_ != null).find(_.id() == name).getOrElse(new Rand(name, -u.getWidth, u.getWidth))
    x.asInstanceOf[Rand]
  }

  // We need a reference to the Parent RandomObj in order to enable or disable a constraint
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
    domainDatabase.foreach(k => k._1.domain.setDomain(k._2))
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
    nOfCalls += 1
    if (!initialize) initializeObject()
    resetDomains()
    preRandomize()
    currentModel.randcVars.foreach(_.next())
    val result = RandBundle.satisfySearch(
      new SimpleSelect[Rand](problemVariables.toArray, null, new IndomainRandom[Rand](currentModel.seed + nOfCalls)),
      listener,
      currentModel
    )
    postRandomize()
    result
  }
}
