package org.jacop.core


/**
  * It creates the largest possible interval.
  */
class BigInterval(var min: BigInt = LongDomain.BigIntMin, var max: BigInt = LongDomain.BigIntMax) extends Cloneable {

  override def clone = new BigInterval(min, max)

  /**
    * It checks equality between intervals.
    *
    * @param interval the inerval to which the comparison is made.
    * @return true if an input interval is equal to this one.
    */
  def eq(interval: BigInterval): Boolean = min == interval.min && max == interval.max

  /**
    * It checks if an intervals contains only one value (singleton).
    *
    * @return true if domain has only one value.
    */
  def singleton: Boolean = min == max

  /**
    * It checks if an intervals contains only value c.
    *
    * @param c integer value to which the singleton is compared to.
    * @return true if variable has a singleton domain and it is equal to value c.
    */
  def singleton(c: BigInt): Boolean = min == max && min == c

  override def toString: String = {
    var result = String.valueOf(min)
    if (max != min) result += ".." + max
    result
  }
}
