package backends.jacop

import org.jacop.constraints.PrimitiveConstraint

/**
 * Since is not all the csp solver have support for conditional constraints, the IfThen and IfThenElse are not part of
 * the common crv package.
 */


/**
  * Helper object for defining a conditional constraint
  */
object IfThenElse {

  /**
    * If then constraint
    * @param ifC the if condition represented as a constraint
    * @param thenC the constraint to be applied if the ifC condition is true
    * @param elseC the constraint to be applied in case the ifC condition is false
    * @param model the current [[Model]], constraint can only be defined inside a [[RandObj]]
    * @return return the newly constructed [[Constraint]]
    */
  def apply(ifC: crv.Constraint)(thenC: crv.Constraint)(elseC: crv.Constraint)(implicit model: Model): Constraint = {
    val newConstraint = new org.jacop.constraints.IfThenElse(
      ifC.getConstraint.asInstanceOf[PrimitiveConstraint],
      thenC.getConstraint.asInstanceOf[PrimitiveConstraint],
      elseC.getConstraint.asInstanceOf[PrimitiveConstraint]
    )
    model.constr += newConstraint
    ifC.disable()
    thenC.disable()
    elseC.disable()
    new Constraint(newConstraint)
  }
}
