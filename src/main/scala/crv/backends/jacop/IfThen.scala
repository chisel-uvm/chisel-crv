package crv.backends.jacop

import org.jacop.constraints.PrimitiveConstraint

/**
  * Since is not all the csp solver have support for conditional constraints, the IfThen and IfThenElse are not part of
  * the common crv package.
  */

/**
  * Helper object for defining IfThen constraints
  */
object IfThen {

  /**
    * If then constraint
    * @param ifC the if condition represented as a constraint
    * @param thenC the constraint to be applied if the ifC condition is true
    * @param model the current [[Model]], constraint can only be defined inside a [[RandObj]]
    * @return return the newly constructed [[Constraint]]
    */
  def apply(ifC: crv.Constraint)(thenC: crv.Constraint)(implicit model: Model): Constraint = {
    val newConstraint =
      new org.jacop.constraints.IfThen(
        ifC.getConstraint.asInstanceOf[PrimitiveConstraint],
        thenC.getConstraint.asInstanceOf[PrimitiveConstraint]
      )
    val crvc = new Constraint(newConstraint)
    model.crvconstr += crvc
    ifC.disable()
    thenC.disable()
    crvc
  }
}
