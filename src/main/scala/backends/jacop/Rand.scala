package backends.jacop

import org.jacop.constraints._
import org.jacop.core.IntDomain
import org.jacop.scala.{IntSet, SetVar}
import org.jacop.set.constraints.{EinA, XinA}

class Rand(name: String, min: Int, max: Int)(implicit val model: Model)
    extends org.jacop.core.IntVar(model, name, min, max)
    with crv.Rand {

  override type U = Rand

  /**
    * Defines an anonymous finite domain integer variable.
    *
    * @constructor Creates a new finite domain integer variable.
    * @param min minimal value of variable's domain.
    * @param max maximal value of variable's domain.
    */
  def this(min: Int, max: Int)(implicit model: Model) = {
    this("_$" + model.n, min, max)(model)
    model.n += 1
  }

  /**
    * Defines an anonymous finite domain integer variable.
    *
    * @constructor Creates a new finite domain integer variable with minimal and maximal
    *              values in the domain defined by [[org.jacop]]
    * @param name variable's identifier.
    */
  def this(name: String)(implicit model: Model) = {
    this(name, org.jacop.core.IntDomain.MinInt, org.jacop.core.IntDomain.MaxInt)(model)
    model.n += 1
  }

  /**
    * Defines an anonymous finite domain integer variable.
    *
    * @constructor Creates a new finite domain integer variable with minimal and maximal
    *              values in the domain defined by [[org.jacop]]
    */
  def this()(implicit model: Model) = {
    this(org.jacop.core.IntDomain.MinInt, org.jacop.core.IntDomain.MaxInt)(model)
    model.n += 1
  }

  /**
    * Defines a finite domain integer variable.
    *
    * @constructor Create a new finite domain integer variable with the domain defined by IntSet.
    * @param dom variable's domain defined as a set of integers IntSet.
    */
  def this(dom: IntSet)(implicit model: Model) = {
    this()(model)
    this.dom.intersectAdapt(dom)
    model.n += 1
  }

  /**
    * Defines a finite domain integer variable.
    *
    * @constructor Create a new finite domain integer variable with the domain
    *              defined by IntSet.
    * @param name variable's identifier.
    * @param dom  variable's domain defined as a set of IntSet.
    */
  def this(name: String, dom: IntSet)(implicit model: Model) = {
    this(name)(model)
    this.dom.intersectAdapt(dom)
    model.n += 1
  }

  /**
    * Assign a specific value to the current variable
    * @param v BigInt
    */
  def setVar(v: BigInt): Unit = {
    require(v < Int.MaxValue)
    setDomain(v.toInt, v.toInt)
  }

  /**
    * Defines the add [[Constraint]] between two Rand variables
    *
    * @param that a second parameter for the addition constraint
    * @return [[Rand]] variable being the result of the addition [[Constraint]].
    */
  def +(that: backends.jacop.Rand): Rand = {
    val result = new Rand(IntDomain.addInt(this.min(), that.min()), IntDomain.addInt(this.max(), that.max()))
    val c = new XplusYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
    * Defines add [[Constraint]] between Rand and an integer value.
    *
    * @param that a second integer parameter for the addition [[Constraint]].
    * @return [[Rand]] variable being the result of the addition [[Constraint]].
    */
  def +(that: BigInt): Rand = {
    require(that <= Int.MaxValue)
    val result = new Rand(IntDomain.addInt(this.min(), that.toInt), IntDomain.addInt(this.max(), that.toInt))
    val c = new XplusCeqZ(this, that.toInt, result)
    model.constr += c
    result
  }

  /**
    * Defines subtract [[Constraint]] between two Rand.
    *
    * @param that a second parameter for the subtraction [[Constraint]].
    * @return [[Rand]] variable being the result of the subtraction [[Constraint]].
    */
  def -(that: backends.jacop.Rand): Rand = {
    val result = new Rand(IntDomain.subtractInt(this.min(), that.max()), IntDomain.subtractInt(this.max(), that.min()))
    val c = new XplusYeqZ(result, that, this)
    model.constr += c
    result
  }

  /**
    * Defines subtract [[Constraint]] between [[Rand]] and an integer value.
    *
    * @param that a second integer parameter for the subtraction [[Constraint]].
    * @return [[Rand]] variable being the result of the subtraction [[Constraint]].
    */
  def -(that: BigInt): Rand = {
    require(that <= Int.MaxValue)
    val result = new Rand(IntDomain.subtractInt(this.min(), that.toInt), IntDomain.subtractInt(this.max(), that.toInt))
    val c = new XplusCeqZ(result, that.toInt, this)
    model.constr += c
    result
  }

  /**
    * Defines equation [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for equation [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #=(that: backends.jacop.Rand): crv.Constraint = {
    val c = new XeqY(this, that)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines equation [[Constraint]] between [[Rand]] and a integer constant.
    *
    * @param that a second parameter for equation [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #=(that: BigInt): crv.Constraint = {
    require(that <= Int.MaxValue)
    val c = new XeqC(this, that.toInt)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines multiplication [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for the multiplication [[Constraint]].
    * @return [[Rand]] variable being the result of the multiplication [[Constraint]].
    */
  def *(that: backends.jacop.Rand): Rand = {
    val bounds = IntDomain.mulBounds(this.min(), this.max(), that.min(), that.max())
    val result = new Rand(bounds.min(), bounds.max())
    val c = new XmulYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
    * Defines multiplication [[Constraint]] between [[Rand]] and an integer value.
    *
    * @param that a second integer parameter for the multiplication [[Constraint]].
    * @return [[Rand]] variable being the result of the multiplication [[Constraint]].
    */
  def *(that: BigInt): Rand = {
    require(that <= Int.MaxValue)
    val bounds = IntDomain.mulBounds(this.min(), this.max(), that.toInt, that.toInt)
    val result = new Rand(bounds.min(), bounds.max())
    val c = new XmulCeqZ(this, that.toInt, result)
    model.constr += c
    result
  }

  /**
    * Defines integer division [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for the integer division [[Constraint]].
    * @return [[Rand]] variable being the result of the integer division [[Constraint]].
    */
  def div(that: backends.jacop.Rand): Rand = {
    val bounds = IntDomain.divBounds(this.min(), this.max(), that.min(), that.max())
    val result = new Rand(bounds.min(), bounds.max())
    val c = new XdivYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
    * Defines integer division [[Constraint]] between [[Rand]] and an integer value.
    *
    * @param that a second parameter for the integer division [[Constraint]].
    * @return [[Rand]] variable being the result of the integer division [[Constraint]].
    */
  def div(that: BigInt): Rand = {
    require(that < Int.MaxValue)
    this.div(new Rand(that.toInt, that.toInt))
  }

  /**
    * Defines [[Constraint]] for integer reminder from division between two [[Rand]].
    *
    * @param that a second parameter for integer reminder from division [[Constraint]].
    * @return [[Rand]] variable being the result of the integer reminder from division [[Constraint]].
    */
  def mod(that: backends.jacop.Rand): Rand = {
    var reminderMin: Int = 0
    var reminderMax: Int = 0

    if (this.min() >= 0) {
      reminderMin = 0
      reminderMax = Math.max(Math.abs(that.min()), Math.abs(that.max())) - 1
    } else if (this.max() < 0) {
      reminderMax = 0
      reminderMin = -Math.max(Math.abs(that.min()), Math.abs(that.max())) + 1
    } else {
      reminderMin = Math.min(Math.min(that.min(), -that.min()), Math.min(that.max(), -that.max())) + 1
      reminderMax = Math.max(Math.max(that.min(), -that.min()), Math.max(that.max(), -that.max())) - 1
    }

    val result = new Rand(reminderMin, reminderMax)
    val c = new XmodYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
    * Defines [[Constraint]] for integer reminder from division [[Rand]] and an integer value.
    *
    * @param that a second parameter for integer reminder from division [[Constraint]].
    * @return [[Rand]] variable being the result of the integer reminder from division [[Constraint]].
    */
  def mod(that: BigInt): Rand = {
    require(that <= Int.MaxValue)
    this.mod(new Rand(that.toInt, that.toInt))
  }

  /**
    * Defines exponentiation [[Constraint]] between two [[Rand]].
    *
    * @param that exponent for the exponentiation [[Constraint]].
    * @return [[Rand]] variable being the result of the exponentiation [[Constraint]].
    */
  def ^(that: backends.jacop.Rand): Rand = {
    val result = new Rand()
    val c = new XexpYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
    * Defines exponentiation [[Constraint]] between [[Rand]] and an integer value.
    *
    * @param that exponent for the exponentiation [[Constraint]].
    * @return [[Rand]] variable being the result of the exponentiation [[Constraint]].
    */
  def ^(that: BigInt): Rand = {
    require(that <= Int.MaxValue)
    this ^ new Rand(that.toInt, that.toInt)
  }

  /**
    * Defines unary "-" [[Constraint]] for [[Rand]].
    *
    * @return the defined [[Constraint]].
    */
  def unary_- : Rand = {
    val result = new Rand(-this.max(), -this.min())
    val c = new XplusYeqC(this, result, 0)
    model.constr += c
    result
  }

  /**
    * Defines inequality [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for inequality [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #\=(that: backends.jacop.Rand): crv.Constraint = {
    val c = new XneqY(this, that)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines inequality [[Constraint]] between [[Rand]] and integer constant.
    *
    * @param that a second parameter for inequality [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #\=(that: BigInt): crv.Constraint = {
    require(that <= Int.MaxValue)
    val c = new XneqC(this, that.toInt)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "less than" [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for "less than" [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #<(that: backends.jacop.Rand): crv.Constraint = {
    val c = new XltY(this, that)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "less than" [[Constraint]] between [[Rand]] and integer constant.
    *
    * @param that a second parameter for "less than" [[Constraint]].
    * @return the equation [[Constraint]].
    */
  def #<(that: BigInt): crv.Constraint = {
    require(that <= Int.MaxValue)
    val c = new XltC(this, that.toInt)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "less than or equal" [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for "less than or equal" [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #<=(that: backends.jacop.Rand): crv.Constraint = {
    val c = new XlteqY(this, that)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "less than or equal" [[Constraint]] between [[Rand]] and integer constant.
    *
    * @param that a second parameter for "less than or equal" [[Constraint]].
    * @return the equation [[Constraint]].
    */
  def #<=(that: BigInt): crv.Constraint = {
    require(that <= Int.MaxValue)
    val c = new XlteqC(this, that.toInt)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "greater than" [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for "greater than" [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #>(that: backends.jacop.Rand): crv.Constraint = {
    val c = new XgtY(this, that)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "greater than" [[Constraint]] between [[Rand]] and integer constant.
    *
    * @param that a second parameter for "greater than" [[Constraint]].
    * @return the equation [[Constraint]].
    */
  def #>(that: BigInt): crv.Constraint = {
    require(that <= Int.MaxValue)
    val c = new XgtC(this, that.toInt)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "greater than or equal" [[Constraint]] between two [[Rand]].
    *
    * @param that a second parameter for "greater than or equal" [[Constraint]].
    * @return the defined [[Constraint]].
    */
  def #>=(that: backends.jacop.Rand): crv.Constraint = {
    val c = new XgteqY(this, that)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines "greater than or equal" [[Constraint]] between [[Rand]] and integer constant.
    *
    * @param that a second parameter for "greater than or equal" [[Constraint]].
    * @return the equation [[Constraint]].
    */
  def #>=(that: BigInt): crv.Constraint = {
    require(that <= Int.MaxValue)
    val c = new XgteqC(this, that.toInt)
    model.constr += c
    new Constraint(c)
  }

  /**
    * Defines [[Constraint]] on inclusion of a [[Rand]] variable value in a set.
    *
    * @param that set that this variable's value must be included.
    * @return the equation [[Constraint]].
    */
  def in(that: SetVar): crv.Constraint = {
    if (min == max) {
      val c = new EinA(min, that)
      model.constr += c
      new Constraint(c)
    } else {
      val c = new XinA(this, that)
      model.constr += c
      new Constraint(c)
    }
  }

  /**
    * Defines [[Constraint]] on inclusion of a [[Rand]] variable value in a set.
    *
    * @param that set that this variable's value must be included.
    * @return the equation [[Constraint]].
    */
  def inside(that: SetVar): crv.Constraint = {
    this.in(that)
  }
}
