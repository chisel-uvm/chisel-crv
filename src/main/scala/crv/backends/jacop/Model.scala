package crv.backends.jacop

import org.jacop.scala.trace

import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Internal class defined only in the jacop backand. This class is used as a database in which all the random variables
  * are stored.
  * @param seed with which the current class is initialized
  */
class Model(val seed: Int = new Random().nextInt()) extends org.jacop.scala.Model {
  val crvconstr = new ListBuffer[crv.backends.jacop.Constraint]()
  val randcVars = new ListBuffer[Randc]

  override def imposeAllConstraints() {
    this.crvconstr.filter(_.isEanble).foreach(e => this.impose(e.getConstraint))
    this.numberOfConstraints = 0
  }

  def apply(s: String): Rand = {
    vars.filter(_ != null).find(_.id() == s).get.asInstanceOf[Rand]
  }
}
