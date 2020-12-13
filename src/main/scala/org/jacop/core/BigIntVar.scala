package org.jacop.core

import org.jacop.constraints.Constraint
import org.jacop.core.BigIntVar.idNumber
import org.jacop.satwrapper.translation.SatCPBridge

import java.util
import java.util.concurrent.atomic.AtomicInteger

object BigIntVar {

  val idNumber = new AtomicInteger(0)
}

class BigIntVar extends Var {

  /**
    * It stores pointer to a current domain, which has stamp equal to store
    * stamp.
    */
  var domain: LongDomain = null

  /**
    * It stores information about the variable in SAT domain.
    */
  var satBridge: SatCPBridge = null

  /**
    * It creates a variable in a given store, with a given name and
    * a given domain.
    *
    * @param store store in which the variable is created.
    * @param name  the name for the variable being created.
    * @param dom   the domain of the variable being created.
    */
  def this(store: Store, name: String, dom: LongDomain) {
    this()
    commonInitialization(store, name, dom)
  }

  private def commonInitialization(store: Store, name: String, dom: LongDomain): Unit = {
    dom.searchConstraints = new util.ArrayList[org.jacop.constraints.Constraint]
    dom.modelConstraints = new Array[Array[Constraint]](LongDomain.eventsInclusion.length)
    dom.modelConstraintsToEvaluate = new Array[Int](LongDomain.eventsInclusion.length)
    assert((name.lastIndexOf(" ") == -(1)), "Name can not contain space character")
    id = name
    domain = dom
    domain.stamp = 0
    index = store.putVariable(this)
    this.store = store
  }

  /**
    * It creates a variable in a given store, with a given name and
    * a given domain.
    *
    * @param store store in which the variable is created.
    * @param dom   the domain of the variable being created.
    */
  def this(store: Store, dom: LongDomain) {
    this(store, store.getVariableIdPrefix + idNumber.incrementAndGet, dom)
  }

  /**
    * This constructor creates a variable with empty domain (standard
    * IntervalDomain domain), automatically generated name, and empty attached
    * constraint list.
    *
    * @param store store in which the variable is created.
    */
  def this(store: Store) {
    this(store, store.getVariableIdPrefix + idNumber.incrementAndGet, new IntervalDomain(5))
  }

  /**
    * This constructor creates a variable with a domain between min..max,
    * automatically generated name, and empty attached constraint list.
    *
    * @param store store in which the variable is created.
    * @param min   the minimum value of the domain.
    * @param max   the maximum value of the domain.
    */
  def this(store: Store, min: Int, max: Int) {
    this(store, store.getVariableIdPrefix + idNumber.incrementAndGet, min, max)
  }

  /**
    * This constructor creates a variable with an empty domain (standard
    * IntervalDomain domain), the specified name, and an empty attached
    * constraint list.
    *
    * @param store store in which the variable is created.
    * @param name  the name for the variable being created.
    */
  def this(store: Store, name: String) {
    this(store, name, new IntervalDomain(5))
  }

  /**
    * This constructor creates a variable in a given store, with
    * the domain specified by min..max and with the given name.
    *
    * @param store the store in which the variable is created.
    * @param name  the name of the variable being created.
    * @param min   the minimum value of the variables domain.
    * @param max   the maximum value of the variables domain.
    */
  def this(store: Store, name: String, min: Int, max: Int) {
    this(store, name, new LongDomain(min, max))
  }

  /**
    * It is possible to add the domain of variable. It should be used with
    * care, only right after variable was created and before it is used in
    * constraints or search. Current implementation requires domains being
    * added in the increasing order (e.g. 1..5 before 9..10).
    *
    * @param min the left bound of the interval being added.
    * @param max the right bound of the interval being added.
    */
  def addDom(min: Int, max: Int): Unit = {
    domain.unionAdapt(min, max)
  }

  /**
    * It is possible to set the domain of variable. It should be used with
    * care, only right after variable was created and before it is used in
    * constraints or search.
    *
    * @param min the left bound of the interval used to set this variable domain to.
    * @param max the right bound of the interval used to set this variable domain to.
    */
  def setDomain(min: Int, max: Int): Unit = {
    domain.setDomain(min, max)
  }

  /**
    * This function returns current value in the domain of the variable. If
    * current domain of variable is not singleton then warning is printed and
    * minimal value is returned.
    *
    * @return the value to which the variable has been grounded to.
    */
  def value: Int = {
    assert(singleton, "Request for a value of not grounded variable " + this)
    // if (!singleton())
    //	Thread.dumpStack();
    return domain.min
  }

  /**
    * It checks if the domain contains only one value equal to c.
    *
    * @param val value to which we compare the singleton of the variable.
    * @return true if a variable domain is singleton and it is equal to the specified value.
    */
  def singleton(`val`: Int): Boolean = {
    return domain.singleton(`val`)
  }

  /**
    * This function returns current maximal value in the domain of the
    * variable.
    *
    * @return the maximum value belonging to the domain.
    */
  def max: Int = {
    return domain.max
  }

  /**
    * This function returns current minimal value in the domain of the
    * variable.
    *
    * @return the minimum value beloning to the domain.
    */
  def min: Int = {
    return domain.min
  }

  /**
    * It is possible to set the domain of variable. It should be used with
    * care, only right after variable was created and before it is used in
    * constraints or search.
    *
    * @param dom domain to which the current variable domain is set to.
    */
  def setDomain(dom: LongDomain): Unit = {
    domain.setDomain(dom)
  }

  /**
    * It is possible to add the domain of variable. It should be used with
    * care, only right after variable was created and before it is used in
    * constraints or search.
    *
    * @param dom the added domain.
    */
  def addDom(dom: LongDomain): Unit = {
    domain.addDom(dom)
  }

  /**
    * This function returns current domain of the variable.
    *
    * @return the domain of the variable.
    */
  override def dom: LongDomain = {
    return domain
  }

  /**
    * It checks if the domains of variables are equal.
    *
    * @param var the variable to which current variable is compared to.
    * @return true if both variables have the same domain.
    */
  def eq(`var`: IntVar): Boolean = {
    return domain.eq(`var`.dom)
  }

  /**
    * It returns the size of the current domain.
    *
    * @return the size of the variables domain.
    */
  override def getSize: Int = {
    return domain.getSize
  }

  override def getSizeFloat: Double = {
    return getSize.toDouble
  }

  /**
    * It checks if the domain is empty.
    *
    * @return true if variable domain is empty.
    */
  override def isEmpty: Boolean = {
    return domain.isEmpty
  }

  /**
    * It registers constraint with current variable, so anytime this variable
    * is changed the constraint is reevaluated. Pruning events constants from 0
    * to n, where n is the strongest pruning event.
    *
    * @param c            the constraint which is being attached to the variable.
    * @param pruningEvent type of the event which must occur to trigger the execution of the consistency function.
    */
  override def putModelConstraint(
    c:            Constraint,
    pruningEvent: Int
  ): Unit = { // If variable is a singleton then it will not be put in the model.
    // It will be put in the queue and evaluated only once in the queue.
    // If constraint is consistent for a singleton then it will remain
    // consistent from the point of view of this variable.
    if (singleton) {
      return
    }
    // if Event is NONE then constraint is not being attached, it will
    // be only evaluated once, as after imposition it is being put in the constraint
    // queue.
    if (pruningEvent == Domain.NONE) {
      return
    }
    domain.putModelConstraint(store.level, this, c, pruningEvent)
    store.recordChange(this)
  }

  /**
    * It registers constraint with current variable, so always when this variable
    * is changed the constraint is reevaluated.
    *
    * @param c the constraint which is added as a search constraint.
    */
  override def putSearchConstraint(c: Constraint): Unit = {
    if (singleton) {
      return
    }
    domain.putSearchConstraint(store.level, this, c)
    store.recordChange(this)
  }

  /**
    * It returns the values which have been removed at current store level. It does
    * _not_ return the recent pruning in between the calls to that function.
    *
    * @return difference between the current level and the one before it.
    */
  def recentDomainPruning: LongDomain = {
    return domain.recentDomainPruning(store.level)
  }

  /**
    * It detaches constraint from the current variable, so change in variable
    * will not cause constraint reevaluation. It is only removed from the
    * current level onwards. Removing current level at later stage will
    * automatically re-attached the constraint to the variable.
    *
    * @param c the constraint being detached from the variable.
    */
  override def removeConstraint(c: Constraint): Unit = {
    if (singleton) {
      return
    }
    var i: Int = domain.searchConstraintsToEvaluate - 1

    while ({
      i >= 0
    }) {
      if (domain.searchConstraints.get(i) eq c) {
        domain.removeSearchConstraint(store.level, this, i, c)
      }
      i -= 1
    }
    if (i == -(1)) {
      domain.removeModelConstraint(store.level, this, c)
    }
    store.recordChange(this)
  }

  /**
    * It checks if the domain contains only one value.
    *
    * @return true if the variable domain is a singleton, false otherwise.
    */
  override def singleton: Boolean = {
    return domain.singleton
  }

  /**
    * It returns current number of constraints which are associated with
    * variable and are not yet satisfied.
    *
    * @return number of constraints attached to the variable.
    */
  override def sizeConstraints: Int = {
    return domain.sizeConstraints
  }

  /**
    * It returns all constraints which are associated with variable, even the
    * ones which are already satisfied.
    *
    * @return number of constraints attached at the earliest level of the variable.
    */
  override def sizeConstraintsOriginal: Int = {
    return domain.sizeConstraintsOriginal
  }

  /**
    * It returns current number of constraints which are associated with
    * variable and are not yet satisfied.
    *
    * @return number of attached search constraints.
    */
  override def sizeSearchConstraints: Int = {
    return domain.searchConstraintsToEvaluate
  }

  /**
    * This function returns stamp of the current domain of variable. It is
    * equal or smaller to the stamp of store. Larger difference indicates that
    * variable has been changed for a longer time.
    *
    * @return level for which the most recent changes have been applied to.
    */
  override def level: Int = {
    return domain.stamp
  }

  override def toString: String = {
    val result: StringBuffer = new StringBuffer(id)
    if (domain.singleton) {
      result.append(" = ")
    } else {
      result.append("::")
    }
    result.append(domain)
    return result.toString
  }

  /**
    * It returns the string representation of the variable using the full representation
    * of the domain.
    *
    * @return string representation.
    */
  override def toStringFull: String = {
    return id + domain.toStringFull
  }

  override def remove(removedLevel: Int): Unit = {
    domain.removeLevel(removedLevel, this)
  }

  /**
    * It informs the variable that its variable has changed according to the specified event.
    *
    * @param event the type of the change (GROUND, BOUND, ANY).
    */
  override def domainHasChanged(event: Int): Unit = {
    assert(
      ((event == LongDomain.ANY && !(singleton)) || (event == LongDomain.BOUND && !(singleton)) || (event == LongDomain.GROUND && singleton)),
      "Wrong event generated"
    )
    store.addChanged(this, event, Integer.MIN_VALUE)
  }

  override def putConstraint(c: Constraint): Unit = {
    putModelConstraint(c, LongDomain.ANY)
  }
}
