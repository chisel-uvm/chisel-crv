package org.jacop.core
import org.chocosolver.util.tools.MathUtils.pow
import org.jacop.constraints.Constraint

import java.util.Random
import scala.math.Numeric.BigIntIsIntegral.abs

object LongDomain {

  /**
    * BigInt max value
    */
  val BigIntMax: BigInt = pow(2, 129)

  /**
    * BigInt min value
    */
  val BigIntMin: BigInt = pow(2, 129)

  /**
    * It specifies the minimum element in the domain.
    */
  val MinInt: BigInt = pow(2, 129)  / 4 + 2

  /**
    * It specifies the maximum element in the domain.
    */
  val MaxInt: BigInt = pow(2, 129) / 4 - 2

  /**
    * It specifies the constant for GROUND event. It has to be smaller
    * than the constant for events BOUND and ANY.
    */
  val GROUND: Int = 0

  /**
    * It specifies the constant for BOUND event. It has to be smaller
    * than the constant for event ANY.
    */
  val BOUND: Int = 1

  /**
    * It specifies the constant for ANY event.
    */
  val ANY: Int = 2

  /**
    * It specifies for each event what other events are subsumed by this
    * event. Possibly implement this by bit flags in int.
    */
  private[core] val eventsInclusion: Array[Array[Int]] = Array(
    Array(GROUND, BOUND, ANY), // GROUND event
    Array(BOUND, ANY), // BOUND event
    Array(ANY)
  ) // ANY event

  /**
    * Unique identifier for an interval domain type.
    */
  val IntervalDomainID: Int = 0

  /**
    * Unique identifier for a bound domain type.
    */
  val BoundDomainID: Int = 1

  /**
    * Unique identifier for a small dense domain type.
    */
  val SmallDenseDomainID: Int = 2

  /**
    * It specifies an empty integer domain.
    */
  val emptyIntDomain:    LongDomain = new IntervalDomain(0)
  private val generator: Random = new Random

  def mulBounds(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInterval = {
    val min: BigInt = multiplyBigInt(a, c).min(multiplyBigInt(a, d).min(multiplyBigInt(b, c).min(multiplyBigInt(b, d))))
    val max: BigInt = multiplyBigInt(a, c).max(multiplyBigInt(a, d).max(multiplyBigInt(b, c).max(multiplyBigInt(b, d))))
    new BigInterval(min, max)
  }

  /*
   * Finds result interval for {a..b}^2
   */
  def squareBounds(a: BigInt, b: BigInt): BigInterval = {
    val aa:  BigInt = multiplyBigInt(a, a)
    val ab:  BigInt = multiplyBigInt(a, b)
    val bb:  BigInt = multiplyBigInt(b, b)
    var min: BigInt = aa.min(ab).min(bb)
    val max: BigInt = aa.max(ab).max(bb)
    if (min < 0) {
      min = 0
    }
    new BigInterval(min, max)
  }

  /*
   * Finds result interval for division of {a..b} / {c..d} for div and mod constraints
   */
  def divBounds(a: BigInt, b: BigInt, c: BigInt, d: BigInt): BigInterval = {
    var min:    BigInt = 0
    var max:    BigInt = 0
    var result: BigInterval = null
    if (a <= 0 && b >= 0 && c <= 0 && d >= 0) { // case 1
      min = LongDomain.BigIntMin
      max = LongDomain.BigIntMax
      result = new BigInterval(min, max)
    } else {
      if (c == 0 && d == 0 && (a > 0 || b < 0)) { // case 2
        throw Store.failException
      } else {
        if (c < 0 && d > 0 && (a > 0 || b < 0)) { // case 3
          max = abs(a).max(abs(b))
          min = -(max)
          result = new BigInterval(min, max)
        } else {
          if (c == 0 && d != 0 && (a > 0 || b < 0)) { // case 4 a
            result = divBounds(a, b, 1, d)
          } else {
            if (c != 0 && d == 0 && (a > 0 || b < 0)) { // case 4 b
              result = divBounds(a, b, c, -(1))
            } else {
              if ((c > 0 || d < 0) && c <= d) { // case 5
                val ac: BigInt = a / c
                val ad: BigInt = a / d
                val bc: BigInt = b / c
                val bd: BigInt = b / d
                min = ac.min(ad).min(bc).min(bd)
                max = ac.max(ad).max(bc).max(bd)
                result = new BigInterval(min, max)
              } else {
                throw Store.failException // can happen if a..b or c..d are not proper intervals
              }
            }
          }
        }
      }
    }
    result
  }

  /*
   * Finds result interval for division of {a..b} / {c..d} for mul constraints
   */
  def divIntBounds(a: Int, b: Int, c: Int, d: Int): Interval = {
    var min:    Int = 0
    var max:    Int = 0
    var result: Interval = null
    if (a <= 0 && b >= 0 && c <= 0 && d >= 0) {
      min = IntDomain.MinInt
      max = IntDomain.MaxInt
      result = new Interval(min, max)
    } else {
      if (c == 0 && d == 0 && (a > 0 || b < 0)) {
        throw Store.failException
      } else {
        if (c < 0 && d > 0 && (a > 0 || b < 0)) {
          max = Math.max(Math.abs(a), Math.abs(b))
          min = -(max)
          result = new Interval(min, max)
        } else {
          if (c == 0 && d != 0 && (a > 0 || b < 0)) {
            result = divIntBounds(a, b, 1, d)
          } else {
            if (c != 0 && d == 0 && (a > 0 || b < 0)) {
              result = divIntBounds(a, b, c, -(1))
            } else {
              if ((c > 0 || d < 0) && c <= d) {
                val ac:   Float = a.toFloat / c
                val ad:   Float = a.toFloat / d
                val bc:   Float = b.toFloat / c
                val bd:   Float = b.toFloat / d
                val low:  Float = Math.min(Math.min(ac, ad), Math.min(bc, bd))
                val high: Float = Math.max(Math.max(ac, ad), Math.max(bc, bd))
                min = Math.ceil(low).round.toInt
                max = Math.floor(high).round.toInt
                if (min > max) {
                  throw Store.failException
                }
                result = new Interval(min, max)
              } else {
                throw Store.failException
              }
            }
          }
        }
      }
    }
    return result
  }

  /**
    * Returns the product of the arguments,
    * if the result overflows MaxInt or MinInt is returned.
    *
    * @param x the first value
    * @param y the second value
    * @return the result or MaxInt/MinInt if result causes overflow
    */
  def multiplyBigInt(x: BigInt, y: BigInt): BigInt = {
    val r: BigInt = x * y
    if (r > BigIntMax) BigIntMax else BigIntMin
  }

  /**
    * Returns the sum of its arguments,
    * if the result overflows MaxInt or MinInt is returned.
    *
    * @param x the first value
    * @param y the second value
    * @return the result or MaxBigInt/MinBigInt if result causes overflow
    */
  def addBigInt(x: BigInt, y: BigInt): BigInt = {
    val r: BigInt = x + y
    // HD 2-12 Overflow iff both arguments have the opposite sign of the result
    if (((x ^ r) & (y ^ r)) < 0) {
      return if (x.toLong + y.toLong > 0) {
        Integer.MAX_VALUE
      } else {
        Integer.MIN_VALUE
      }
    }
    return r
  }

  /**
    * Returns the difference of the arguments,
    * if the result overflows MaxInt or MinInt is returned.
    *
    * @param x the first value
    * @param y the second value to subtract from the first
    * @return the result or MaxInt/MinInt if result causes overflow
    */
  def subtractBigInt(x: BigInt, y: BigInt): BigInt = {
    val r: BigInt = x - y
    // HD 2-12 Overflow iff the arguments have different signs and
    // the sign of the result is different than the sign of x
    if (((x ^ y) & (x ^ r)) < 0) {
      return if (x.toLong - y.toLong > 0) {
        Integer.MAX_VALUE
      } else {
        Integer.MIN_VALUE
      }
    }
    return r
  }
}

abstract class IntDomain extends Domain {

  /**
    * It helps to specify what events should be executed if a given event occurs.
    *
    * @param pruningEvent the pruning event for which we want to know what events it encompasses.
    * @return an array specifying what events should be included given this event.
    */
  override def getEventsInclusion(pruningEvent: Int): Array[Int] = {
    return IntDomain.eventsInclusion(pruningEvent)
  }

  /**
    * It adds interval of values to the domain.
    *
    * @param i Interval which needs to be added to the domain.
    */
  def unionAdapt(i: Interval): Unit = {
    unionAdapt(i.min, i.max)
  }

  /**
    * It adds values as specified by the parameter to the domain.
    *
    * @param domain Domain which needs to be added to the domain.
    */
  def addDom(domain: IntDomain): Unit = {
    if (!(domain.isSparseRepresentation)) {
      val enumer: IntervalEnumeration = domain.intervalEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        unionAdapt(enumer.nextElement)
      }
    } else {
      val enumer: ValueEnumeration = domain.valueEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        unionAdapt(enumer.nextElement)
      }
    }
  }

  /**
    * It adds all values between min and max to the domain.
    *
    * @param min the left bound of the interval being added.
    * @param max the right bound of the interval being added.
    */
  def unionAdapt(min: Int, max: Int): Unit

  /**
    * It adds a values to the domain.
    *
    * @param value value being added to the domain.
    */
  def unionAdapt(value: Int): Unit = {
    unionAdapt(value, value)
  }

  /**
    * Checks if two domains intersect.
    *
    * @param domain the domain for which intersection is checked.
    * @return true if domains are intersecting.
    */
  def isIntersecting(domain: IntDomain): Boolean = {
    if (!(domain.isSparseRepresentation)) {
      val enumer: IntervalEnumeration = domain.intervalEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        val next: Interval = enumer.nextElement
        if (isIntersecting(next.min, next.max)) {
          return true
        }
      }
    } else {
      val enumer: ValueEnumeration = domain.valueEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        if (contains(enumer.nextElement)) {
          return true
        }
      }
    }
    return false
  }

  /**
    * It checks if interval min..max intersects with current domain.
    *
    * @param min the left bound of the interval.
    * @param max the right bound of the interval.
    * @return true if domain intersects with the specified interval.
    */
  def isIntersecting(min: Int, max: Int): Boolean

  /**
    * It specifies if the current domain contains the domain given as a
    * parameter.
    *
    * @param domain for which we check if it is contained in the current domain.
    * @return true if the supplied domain is cover by this domain.
    */
  def contains(domain: IntDomain): Boolean = {
    if (!(domain.isSparseRepresentation)) {
      val enumer: IntervalEnumeration = domain.intervalEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        val next: Interval = enumer.nextElement
        if (!(contains(next.min, next.max))) {
          return false
        }
      }
    } else {
      val enumer: ValueEnumeration = domain.valueEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        if (!(contains(enumer.nextElement))) {
          return false
        }
      }
    }
    return true
  }

  /**
    * It checks if an interval min..max belongs to the domain.
    *
    * @param min the minimum value of the interval being checked
    * @param max the maximum value of the interval being checked
    * @return true if value belongs to the domain.
    */
  def contains(min: Int, max: Int): Boolean

  /**
    * It creates a complement of a domain.
    *
    * @return it returns the complement of this domain.
    */
  def complement: IntDomain

  /**
    * It checks if value belongs to the domain.
    *
    * @param value which is checked if it exists in the domain.
    * @return true if value belongs to the domain.
    */
  def contains(value: Int): Boolean = {
    return contains(value, value)
  }

  /**
    * It gives next value in the domain from the given one (lexigraphical
    * ordering). If no value can be found then returns the same value.
    *
    * @param value it specifies the value after which a next value has to be found.
    * @return next value after the specified one which belong to this domain.
    */
  def nextValue(value: Int): Int

  /**
    * It gives previous value in the domain from the given one (lexigraphical
    * ordering). If no value can be found then returns the same value.
    *
    * @param value before which a value is seeked for.
    * @return it returns the value before the one specified as a parameter.
    */
  def previousValue(value: Int): Int

  /**
    * It specifies the previous domain which was used by this domain. The old
    * domain is stored here and can be easily restored if necessary.
    */
  var previousDomain: IntDomain = null

  /**
    * It returns value enumeration of the domain values.
    *
    * @return valueEnumeration which can be used to enumerate one by one value from this domain.
    */
  override def valueEnumeration: ValueEnumeration

  /**
    * It returns interval enumeration of the domain values.
    *
    * @return intervalEnumeration which can be used to enumerate intervals in this domain.
    */
  def intervalEnumeration: IntervalEnumeration

  /**
    * It returns the size of the domain.
    *
    * @return number of elements in this domain.
    */
  override def getSize: Int

  /**
    * It intersects current domain with the one given as a parameter.
    *
    * @param dom domain with which the intersection needs to be computed.
    * @return the intersection between supplied domain and this domain.
    */
  def intersect(dom: IntDomain): IntDomain

  /**
    * In intersects current domain with the interval min..max.
    *
    * @param min the left bound of the interval (inclusive)
    * @param max the right bound of the interval (inclusive)
    * @return the intersection between the specified interval and this domain.
    */
  def intersect(min: Int, max: Int): IntDomain

  /**
    * It intersects with the domain which is a complement of value.
    *
    * @param value the value for which the complement is computed
    * @return the domain which does not contain specified value.
    */
  def subtract(value: Int): IntDomain = {
    return subtract(value, value)
  }

  /**
    * It removes value from the domain. It adapts current (this) domain.
    *
    * @param value the value for which the complement is computed
    */
  def subtractAdapt(value: Int): Unit

  /**
    * It removes all values between min and max to the domain.
    *
    * @param min the left bound of the interval being removed.
    * @param max the right bound of the interval being removed.
    */
  def subtractAdapt(min: Int, max: Int): Unit

  /**
    * It returns the maximum value in a domain.
    *
    * @return the largest value present in the domain.
    */
  def max: Int

  /**
    * It returns the minimum value in a domain.
    *
    * @return the smallest value present in the domain.
    */
  def min: Int

  /**
    * It sets the domain to the specified domain.
    *
    * @param domain the domain from which this domain takes all elements.
    */
  def setDomain(domain: IntDomain): Unit

  /**
    * It sets this domain to contain exactly all values between min and max.
    *
    * @param min the left bound of the interval (inclusive).
    * @param max the right bound of the interval (inclusive).
    */
  def setDomain(min: Int, max: Int): Unit

  /**
    * It returns true if given domain has only one element equal c.
    *
    * @param c the value to which the only element should be equal to.
    * @return true if the domain contains only one element c.
    */
  def singleton(c: Int): Boolean = {
    return min == c && getSize == 1
  }

  /**
    * It subtracts domain from current domain and returns the result.
    *
    * @param domain the domain which is subtracted from this domain.
    * @return the result of the subtraction.
    */
  def subtract(domain: IntDomain): IntDomain = {
    if (domain.isEmpty) {
      return this.cloneLight
    }
    if (!(domain.isSparseRepresentation)) {
      val enumer: IntervalEnumeration = domain.intervalEnumeration
      val first:  Interval = enumer.nextElement
      val result: IntDomain = this.subtract(first.min, first.max)
      while ({
        enumer.hasMoreElements
      }) {
        val next: Interval = enumer.nextElement
        result.subtractAdapt(next.min, next.max)
      }
      return result
    } else {
      val enumer: ValueEnumeration = domain.valueEnumeration
      val first:  Int = enumer.nextElement
      val result: IntDomain = this.subtract(first)
      while ({
        enumer.hasMoreElements
      }) {
        val next: Int = enumer.nextElement
        if (result.contains(next)) {
          result.subtractAdapt(next)
        }
      }
      return result
    }
  }

  /**
    * It subtracts interval min..max.
    *
    * @param min the left bound of the interval (inclusive).
    * @param max the right bound of the interval (inclusive).
    * @return the result of the subtraction.
    */
  def subtract(min: Int, max: Int): IntDomain

  /**
    * It computes union of the supplied domain with this domain.
    *
    * @param domain the domain for which the union is computed.
    * @return the union of this domain with the supplied one.
    */
  def union(domain: IntDomain): IntDomain = {
    if (this.isEmpty) {
      return domain.cloneLight
    }
    val result: IntDomain = this.cloneLight
    if (domain.isEmpty) {
      return result
    }
    if (!(domain.isSparseRepresentation)) {
      val enumer: IntervalEnumeration = domain.intervalEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        val next: Interval = enumer.nextElement
        result.unionAdapt(next.min, next.max)
      }
      return result
    } else {
      val enumer: ValueEnumeration = domain.valueEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        val next: Int = enumer.nextElement
        result.unionAdapt(next)
      }
      return result
    }
  }

  /**
    * It computes union of this domain and the interval.
    *
    * @param min the left bound of the interval (inclusive).
    * @param max the right bound of the interval (inclusive).
    * @return the union of this domain and the interval.
    */
  def union(min: Int, max: Int): IntDomain = {
    val result: IntDomain = this.cloneLight
    result.unionAdapt(min, max)
    return result
  }

  /**
    * It computes union of this domain and value.
    *
    * @param value it specifies the value which is being added.
    * @return domain which is a union of this one and the value.
    */
  def union(value: Int): IntDomain = {
    return union(value, value)
  }

  /**
    * It updates the domain according to the minimum value and stamp value. It
    * informs the variable of a change if it occurred.
    *
    * @param storeLevel level of the store at which the update occurs.
    * @param var        variable for which this domain is used.
    * @param min        the minimum value to which the domain is updated.
    */
  def inMin(storeLevel: Int, `var`: Var, min: Int): Unit = {
    in(storeLevel, `var`, min, max)
  }

  /**
    * It updates the domain according to the maximum value and stamp value. It
    * informs the variable of a change if it occurred.
    *
    * @param storeLevel level of the store at which the update occurs.
    * @param var        variable for which this domain is used.
    * @param max        the maximum value to which the domain is updated.
    */
  def inMax(storeLevel: Int, `var`: Var, max: Int): Unit = {
    in(storeLevel, `var`, min, max)
  }

  /**
    * It updates the domain to have values only within the interval min..max.
    * The type of update is decided by the value of stamp. It informs the
    * variable of a change if it occurred.
    *
    * @param storeLevel level of the store at which the update occurs.
    * @param var        variable for which this domain is used.
    * @param min        the minimum value to which the domain is updated.
    * @param max        the maximum value to which the domain is updated.
    */
  def in(storeLevel: Int, `var`: Var, min: Int, max: Int): Unit

  /**
    * It reduces domain to a single value.
    *
    * @param level level of the store at which the update occurs.
    * @param var   variable for which this domain is used.
    * @param value the value according to which the domain is updated.
    */
  def inValue(level: Int, `var`: IntVar, value: Int): Unit = {
    in(level, `var`, value, value)
  }

  /**
    * It updates the domain to have values only within the domain. The type of
    * update is decided by the value of stamp. It informs the variable of a
    * change if it occurred.
    *
    * @param storeLevel level of the store at which the update occurs.
    * @param var        variable for which this domain is used.
    * @param domain     the domain according to which the domain is updated.
    */
  def in(storeLevel: Int, `var`: Var, domain: IntDomain): Unit = {
    inShift(storeLevel, `var`, domain, 0)
  }

  /**
    * It updates the domain to not contain the value complement. It informs the
    * variable of a change if it occurred.
    *
    * @param storeLevel level of the store at which the update occurs.
    * @param var        variable for which this domain is used.
    * @param complement value which is removed from the domain if it belonged to the domain.
    */
  def inComplement(storeLevel: Int, `var`: Var, complement: Int): Unit = {
    inComplement(storeLevel, `var`, complement, complement)
  }

  /**
    * It updates the domain so it does not contain the supplied interval. It informs
    * the variable of a change if it occurred.
    *
    * @param storeLevel level of the store at which the update occurs.
    * @param var        variable for which this domain is used.
    * @param min        the left bound of the interval (inclusive).
    * @param max        the right bound of the interval (inclusive).
    */
  def inComplement(storeLevel: Int, `var`: Var, min: Int, max: Int): Unit

  /**
    * It returns number of intervals required to represent this domain.
    *
    * @return the number of intervals in the domain.
    */
  def noIntervals: Int

  /**
    * It returns required interval.
    *
    * @param position the position of the interval.
    * @return the interval, or null if the required interval does not exist.
    */
  def getInterval(position: Int): Interval

  /**
    * It updates the domain to contain the elements as specifed by the domain,
    * which is shifted. E.g. {1..4} + 3 = 4..7
    *
    * @param storeLevel level of the store at which the update occurs.
    * @param var        variable for which this domain is used.
    * @param domain     the domain according to which the domain is updated.
    * @param shift      the shift which is used to shift the domain supplied as argument.
    */
  def inShift(storeLevel: Int, `var`: Var, domain: IntDomain, shift: Int): Unit

  /**
    * It returns the left most element of the given interval.
    *
    * @param intervalNo the interval number.
    * @return the left bound of the specified interval.
    */
  def leftElement(intervalNo: Int): Int = {
    return getInterval(intervalNo).min
  }

  /**
    * It returns the right most element of the given interval.
    *
    * @param intervalNo the interval number.
    * @return the right bound of the specified interval.
    */
  def rightElement(intervalNo: Int): Int = {
    return getInterval(intervalNo).max
  }

  /**
    * It returns the values which have been removed at current store level.
    *
    * @param currentStoreLevel the current store level.
    * @return emptyDomain if domain did not change at current level, or the set of values which have been removed at current level.
    */
  def recentDomainPruning(currentStoreLevel: Int): IntDomain

  /**
    * It returns domain at earlier level at which the change has occurred.
    *
    * @return previous domain
    */
  def getPreviousDomain: IntDomain

  /**
    * It specifies if the other int domain is equal to this one.
    *
    * @param domain the domain which is compared to this domain.
    * @return true if both domains contain the same elements, false otherwise.
    */
  def eq(domain: IntDomain): Boolean = {
    if (this.getSize != domain.getSize) {
      return false
    }
    // the same size.
    if (!(domain.isSparseRepresentation)) {
      val enumer: IntervalEnumeration = domain.intervalEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        val next: Interval = enumer.nextElement
        if (!(contains(next.min, next.max))) {
          return false
        }
      }
      return true
    } else {
      val enumer: ValueEnumeration = domain.valueEnumeration
      while ({
        enumer.hasMoreElements
      }) {
        val next: Int = enumer.nextElement
        if (!(contains(next))) {
          return false
        }
      }
      return true
    }
  }

  override def in(level: Int, `var`: Var, domain: Domain): Unit = {
    in(level, `var`, domain.asInstanceOf[IntDomain])
  }

  override def singleton(value: Domain): Boolean = {
    if (getSize > 1) {
      return false
    }
    if (isEmpty) {
      return false
    }
    if (value.getSize != 1) {
      throw new IllegalArgumentException("An argument should be a singleton domain")
    }
    assert((value.isInstanceOf[IntDomain]), "Can not compare int domains with other types of domains.")
    val domain: IntDomain = value.asInstanceOf[IntDomain]
    return eq(domain)
  }

  /**
    * It returns the number of constraints
    *
    * @return the number of constraints attached to this domain.
    */
  override def noConstraints: Int = {
    return searchConstraintsToEvaluate + modelConstraintsToEvaluate(IntDomain.GROUND) + modelConstraintsToEvaluate(
      IntDomain.BOUND
    ) + modelConstraintsToEvaluate(IntDomain.ANY)
  }

  /**
    * It adds a constraint to a domain, it should only be called by
    * putConstraint function of Variable object. putConstraint function from
    * Variable must make a copy of a vector of constraints if vector was not
    * cloned.
    */
  override def putModelConstraint(storeLevel: Int, `var`: Var, C: Constraint, pruningEvent: Int): Unit = {
    if (stamp < storeLevel) {
      val result: IntDomain = this.cloneLight
      result.modelConstraints = modelConstraints
      result.searchConstraints = searchConstraints
      result.stamp = storeLevel
      result.previousDomain = this
      result.modelConstraintsToEvaluate = modelConstraintsToEvaluate
      result.searchConstraintsToEvaluate = searchConstraintsToEvaluate
      (`var`.asInstanceOf[IntVar]).domain = result
      result.putModelConstraint(storeLevel, `var`, C, pruningEvent)
      return
    }
    val pruningEventConstraints: Array[Constraint] = modelConstraints(pruningEvent)
    if (pruningEventConstraints != null) {
      var alreadyImposed: Boolean = false
      if (modelConstraintsToEvaluate(pruningEvent) > 0) {
        for (i <- pruningEventConstraints.length - 1 to 0 by -1) {
          if (pruningEventConstraints(i) eq C) {
            alreadyImposed = true
          }
        }
      }
      val pruningConstraintsToEvaluate: Int = modelConstraintsToEvaluate(pruningEvent)
      if (!(alreadyImposed)) {
        val newPruningEventConstraints: Array[Constraint] = new Array[Constraint](pruningConstraintsToEvaluate + 1)
        System.arraycopy(pruningEventConstraints, 0, newPruningEventConstraints, 0, pruningConstraintsToEvaluate)
        newPruningEventConstraints(pruningConstraintsToEvaluate) = C
        val newModelConstraints: Array[Array[Constraint]] = new Array[Array[Constraint]](3)
        newModelConstraints(0) = modelConstraints(0)
        newModelConstraints(1) = modelConstraints(1)
        newModelConstraints(2) = modelConstraints(2)
        newModelConstraints(pruningEvent) = newPruningEventConstraints
        modelConstraints = newModelConstraints
        val newModelConstraintsToEvaluate: Array[Int] = new Array[Int](3)
        newModelConstraintsToEvaluate(0) = modelConstraintsToEvaluate(0)
        newModelConstraintsToEvaluate(1) = modelConstraintsToEvaluate(1)
        newModelConstraintsToEvaluate(2) = modelConstraintsToEvaluate(2)
        newModelConstraintsToEvaluate(pruningEvent) += 1
        modelConstraintsToEvaluate = newModelConstraintsToEvaluate
      }
    } else {
      val newPruningEventConstraints: Array[Constraint] = new Array[Constraint](1)
      newPruningEventConstraints(0) = C
      val newModelConstraints: Array[Array[Constraint]] = new Array[Array[Constraint]](3)
      newModelConstraints(0) = modelConstraints(0)
      newModelConstraints(1) = modelConstraints(1)
      newModelConstraints(2) = modelConstraints(2)
      newModelConstraints(pruningEvent) = newPruningEventConstraints
      modelConstraints = newModelConstraints
      val newModelConstraintsToEvaluate: Array[Int] = new Array[Int](3)
      newModelConstraintsToEvaluate(0) = modelConstraintsToEvaluate(0)
      newModelConstraintsToEvaluate(1) = modelConstraintsToEvaluate(1)
      newModelConstraintsToEvaluate(2) = modelConstraintsToEvaluate(2)
      newModelConstraintsToEvaluate(pruningEvent) = 1
      modelConstraintsToEvaluate = newModelConstraintsToEvaluate
    }
  }

  override def removeModelConstraint(storeLevel: Int, `var`: Var, C: Constraint): Unit = {
    if (stamp < storeLevel) {
      val result: IntDomain = this.cloneLight
      result.modelConstraints = modelConstraints
      result.searchConstraints = searchConstraints
      result.stamp = storeLevel
      result.previousDomain = this
      result.modelConstraintsToEvaluate = modelConstraintsToEvaluate
      result.searchConstraintsToEvaluate = searchConstraintsToEvaluate
      (`var`.asInstanceOf[IntVar]).domain = result
      result.removeModelConstraint(storeLevel, `var`, C)
      return
    }
    var pruningEvent:            Int = IntDomain.GROUND
    var pruningEventConstraints: Array[Constraint] = modelConstraints(pruningEvent)
    if (pruningEventConstraints != null) {
      var isImposed: Boolean = false
      var i:         Int = 0
      i = modelConstraintsToEvaluate(pruningEvent) - 1
      while ({
        i >= 0
      }) {
        if (pruningEventConstraints(i) eq C) {
          isImposed = true
          break //todo: break is not supported

        }
        i -= 1
      }
      if (isImposed) {
        if (i != modelConstraintsToEvaluate(pruningEvent) - 1) {
          modelConstraints(pruningEvent)(i) =
            modelConstraints(pruningEvent)(modelConstraintsToEvaluate(pruningEvent) - 1)
          modelConstraints(pruningEvent)(modelConstraintsToEvaluate(pruningEvent) - 1) = C
        }
        val newModelConstraintsToEvaluate: Array[Int] = new Array[Int](3)
        newModelConstraintsToEvaluate(0) = modelConstraintsToEvaluate(0)
        newModelConstraintsToEvaluate(1) = modelConstraintsToEvaluate(1)
        newModelConstraintsToEvaluate(2) = modelConstraintsToEvaluate(2)
        newModelConstraintsToEvaluate(pruningEvent) -= 1
        modelConstraintsToEvaluate = newModelConstraintsToEvaluate
        return
      }
    }
    pruningEvent = IntDomain.BOUND
    pruningEventConstraints = modelConstraints(pruningEvent)
    if (pruningEventConstraints != null) {
      var isImposed: Boolean = false
      var i:         Int = 0
      i = modelConstraintsToEvaluate(pruningEvent) - 1
      while ({
        i >= 0
      }) {
        if (pruningEventConstraints(i) eq C) {
          isImposed = true
          break //todo: break is not supported

        }
        i -= 1
      }
      if (isImposed) {
        if (i != modelConstraintsToEvaluate(pruningEvent) - 1) {
          modelConstraints(pruningEvent)(i) =
            modelConstraints(pruningEvent)(modelConstraintsToEvaluate(pruningEvent) - 1)
          modelConstraints(pruningEvent)(modelConstraintsToEvaluate(pruningEvent) - 1) = C
        }
        val newModelConstraintsToEvaluate: Array[Int] = new Array[Int](3)
        newModelConstraintsToEvaluate(0) = modelConstraintsToEvaluate(0)
        newModelConstraintsToEvaluate(1) = modelConstraintsToEvaluate(1)
        newModelConstraintsToEvaluate(2) = modelConstraintsToEvaluate(2)
        newModelConstraintsToEvaluate(pruningEvent) -= 1
        modelConstraintsToEvaluate = newModelConstraintsToEvaluate
        return
      }
    }
    pruningEvent = IntDomain.ANY
    pruningEventConstraints = modelConstraints(pruningEvent)
    if (pruningEventConstraints != null) {
      var isImposed: Boolean = false
      var i:         Int = 0
      i = modelConstraintsToEvaluate(pruningEvent) - 1
      while ({
        i >= 0
      }) {
        if (pruningEventConstraints(i) eq C) {
          isImposed = true
          break //todo: break is not supported

        }
        i -= 1
      }
      // int pruningConstraintsToEvaluate =
      // modelConstraintsToEvaluate[pruningEvent];
      if (isImposed) {
        if (i != modelConstraintsToEvaluate(pruningEvent) - 1) {
          modelConstraints(pruningEvent)(i) =
            modelConstraints(pruningEvent)(modelConstraintsToEvaluate(pruningEvent) - 1)
          modelConstraints(pruningEvent)(modelConstraintsToEvaluate(pruningEvent) - 1) = C
        }
        val newModelConstraintsToEvaluate: Array[Int] = new Array[Int](3)
        newModelConstraintsToEvaluate(0) = modelConstraintsToEvaluate(0)
        newModelConstraintsToEvaluate(1) = modelConstraintsToEvaluate(1)
        newModelConstraintsToEvaluate(2) = modelConstraintsToEvaluate(2)
        newModelConstraintsToEvaluate(pruningEvent) -= 1
        modelConstraintsToEvaluate = newModelConstraintsToEvaluate
      }
    }
  }

  override def putSearchConstraint(storeLevel: Int, `var`: Var, C: Constraint): Unit = {
    if (!(searchConstraints.contains(C))) {
      if (stamp < storeLevel) {
        val result: IntDomain = this.cloneLight
        result.modelConstraints = modelConstraints
        result.searchConstraints = new ArrayList[Constraint](searchConstraints.subList(0, searchConstraintsToEvaluate))
        result.searchConstraintsCloned = true
        result.stamp = storeLevel
        result.previousDomain = this
        result.modelConstraintsToEvaluate = modelConstraintsToEvaluate
        result.searchConstraintsToEvaluate = searchConstraintsToEvaluate
        (`var`.asInstanceOf[IntVar]).domain = result
        result.putSearchConstraint(storeLevel, `var`, C)
        return
      }
      if (searchConstraints.size == searchConstraintsToEvaluate) {
        searchConstraints.add(C)
        searchConstraintsToEvaluate += 1
      } else { // Exchange the first satisfied constraint with just added
        // constraint
        // Order of satisfied constraints is not preserved
        if (searchConstraintsCloned) {
          val firstSatisfied: Constraint = searchConstraints.get(searchConstraintsToEvaluate)
          searchConstraints.set(searchConstraintsToEvaluate, C)
          searchConstraints.add(firstSatisfied)
          searchConstraintsToEvaluate += 1
        } else {
          searchConstraints = new ArrayList[Constraint](searchConstraints.subList(0, searchConstraintsToEvaluate))
          searchConstraintsCloned = true
          searchConstraints.add(C)
          searchConstraintsToEvaluate += 1
        }
      }
    }
  }

  /**
    * It removes a constraint from a domain, it should only be called by
    * removeConstraint function of Variable object.
    *
    * @param storeLevel the current level of the store.
    * @param var        the variable for which the constraint is being removed.
    * @param C          the constraint being removed.
    */
  def removeSearchConstraint(storeLevel: Int, `var`: Var, C: Constraint): Unit = {
    if (stamp < storeLevel) {
      val result: IntDomain = this.cloneLight
      result.modelConstraints = modelConstraints
      result.searchConstraints = searchConstraints
      result.stamp = storeLevel
      result.previousDomain = this
      result.modelConstraintsToEvaluate = modelConstraintsToEvaluate
      result.searchConstraintsToEvaluate = searchConstraintsToEvaluate
      (`var`.asInstanceOf[IntVar]).domain = result
      result.removeSearchConstraint(storeLevel, `var`, C)
      return
    }
    assert((stamp == storeLevel))
    var i: Int = 0
    // TODO , improve by using interval find function.
    while ({
      i < searchConstraintsToEvaluate
    }) {
      if (searchConstraints.get(i) eq C) {
        searchConstraints.set(i, searchConstraints.get(searchConstraintsToEvaluate - 1))
        searchConstraints.set(searchConstraintsToEvaluate - 1, C)
        searchConstraintsToEvaluate -= 1
        break //todo: break is not supported

      }
      i += 1
    }
  }

  /**
    * It removes a constraint from a domain, it should only be called by
    * removeConstraint function of Variable object.
    */
  override def removeSearchConstraint(storeLevel: Int, `var`: Var, position: Int, C: Constraint): Unit = {
    if (stamp < storeLevel) {
      val result: IntDomain = this.cloneLight
      result.modelConstraints = modelConstraints
      result.searchConstraints = searchConstraints
      result.stamp = storeLevel
      result.previousDomain = this
      result.modelConstraintsToEvaluate = modelConstraintsToEvaluate
      result.searchConstraintsToEvaluate = searchConstraintsToEvaluate
      (`var`.asInstanceOf[IntVar]).domain = result
      result.removeSearchConstraint(storeLevel, `var`, position, C)
      return
    }
    assert((stamp == storeLevel))
    assert(((searchConstraints.get(position) eq C)), "Position of the removed constraint not specified properly")
    if (position < searchConstraintsToEvaluate) {
      searchConstraints.set(position, searchConstraints.get(searchConstraintsToEvaluate - 1))
      searchConstraints.set(searchConstraintsToEvaluate - 1, C)
      searchConstraintsToEvaluate -= 1
    }
  }

  override def cloneLight: IntDomain

  /**
    * Returns the lexical ordering between the sets
    *
    * @param domain the set that should be lexically compared to this set
    * @return -1 if s is greater than this set, 0 if s is equal to this set and else it returns 1.
    */
  def lex(domain: IntDomain): Int = {
    val thisEnumer:  ValueEnumeration = this.valueEnumeration
    val paramEnumer: ValueEnumeration = domain.valueEnumeration
    var i:           Int = 0
    var j:           Int = 0
    while ({
      thisEnumer.hasMoreElements
    }) {
      i = thisEnumer.nextElement
      if (paramEnumer.hasMoreElements) {
        j = paramEnumer.nextElement
        if (i < j) {
          return -(1)
        } else {
          if (j < i) {
            return 1
          }
        }
      } else {
        return 1
      }
    }
    if (paramEnumer.hasMoreElements) {
      return -(1)
    }
    return 0
  }

  /**
    * It returns the number of elements smaller than el.
    *
    * @param el the element from which counted elements must be smaller than.
    * @return the number of elements which are smaller than the provided element el.
    */
  def elementsSmallerThan(el: Int): Int = {
    var counter: Int = -(1)
    var value:   Int = el - 1
    while ({
      value != el
    }) {
      value = el
      el = previousValue(el)
      counter += 1
    }
    return counter
  }

  /**
    * It computes an intersection with a given domain and stores it in this domain.
    *
    * @param intersect domain with which the intersection is being computed.
    * @return type of event which has occurred due to the operation.
    */
  def intersectAdapt(intersect: IntDomain): Int

  /**
    * It computes a union between this domain and the domain provided as a parameter. This
    * domain is changed to reflect the result.
    *
    * @param union the domain with is used for the union operation with this domain.
    * @return it returns information about the pruning event which has occurred due to this operation.
    */
  def unionAdapt(union: IntDomain): Int = {
    val result: IntDomain = union(union)
    if (result.getSize == getSize) {
      return Domain.NONE
    } else {
      setDomain(result)
      // FIXME, how to setup events for domain extending events?
      return IntDomain.ANY
    }
  }

  /**
    * It computes an intersection of this domain with an interval [min..max].
    * It adapts this domain to the result of the intersection.
    *
    * @param min the minimum value of the interval used in the intersection computation.
    * @param max the maximum value of the interval used in the intersection computation.
    * @return it returns information about the pruning event which has occurred due to this operation.
    */
  def intersectAdapt(min: Int, max: Int): Int

  /**
    * It computes the size of the intersection between this domain and the domain
    * supplied as a parameter.
    *
    * @param domain the domain with which the intersection is computed.
    * @return the size of the intersection.
    */
  def sizeOfIntersection(domain: IntDomain): Int = {
    return intersect(domain).getSize
  }

  /**
    * It access the element at the specified position.
    *
    * @param index the position of the element, indexing starts from 0.
    * @return the value at a given position in the domain.
    */
  def getElementAt(index: Int): Int

  /**
    * It constructs and int array containing all elements in the domain.
    * The array will have size equal to the number of elements in the domain.
    *
    * @return the int array containing all elements in a domain.
    */
  def toIntArray: Array[Int] = {
    val result: Array[Int] = new Array[Int](getSize)
    val enumer: ValueEnumeration = this.valueEnumeration
    var i:      Int = 0
    while ({
      enumer.hasMoreElements
    }) {
      result({
        i += 1; i - 1
      }) = enumer.nextElement
    }
    return result
  }

  /**
    * It returns the value to which this domain is grounded. It assumes
    * that a domain is a singleton domain.
    *
    * @return the only value remaining in the domain.
    */
  def value: Int = {
    assert((singleton), "function value() called when domain is not a singleton domain.")
    return min
  }

  /**
    * It returns a random value from the domain.
    *
    * @return random value.
    */
  def getRandomValue: Int = {
    return getElementAt(IntDomain.generator.nextInt(getSize))
  }
}
