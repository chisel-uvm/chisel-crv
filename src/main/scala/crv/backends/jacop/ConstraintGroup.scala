package crv.backends.jacop

class ConstraintGroup(group: Constraint*) extends crv.ConstraintGroup {

  /**
    * List of all the constraint declared in the group
    */
  override val constraints: List[Constraint] = group.toList
}
