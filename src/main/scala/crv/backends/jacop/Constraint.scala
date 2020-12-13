package crv.backends.jacop

import java.util

import org.jacop.constraints.PrimitiveConstraint
import org.jacop.core.{Store, Var}

class Constraint(private val constraint: org.jacop.constraints.Constraint)(implicit var model: Model)
    extends crv.Constraint {

  override type U = org.jacop.constraints.Constraint
  var isEanble: Boolean = true
  /**
    * Disable the current constraint
    */
  override def disable(): Unit = {
    isEanble = false
    constraint.removeConstraint()
  }

  /**
    * Enables the current constraint. Each constraint is by default enable when instantiated. This method has effect only
    * if called after [[disable]]
    */
  override def enable(): Unit = {
    isEanble = true
    model.addChanged(constraint)
  }

  /**
    * Returns the current constraint. This method was introduced as an helper class for the jacop backend
    * @return [[Constraint]]
    */
  override def getConstraint: org.jacop.constraints.Constraint = constraint

  override def toString: String = constraint.toString

}
