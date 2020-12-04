package backends.jacop

import scala.util.Random

/**
  * Internal class defined only in the jacop backand. This class is used as a database in which all the random variables
  * are stored.
  * @param seed with which the current class is initialized
  */
class Model(val seed: Int = new Random().nextInt()) extends org.jacop.scala.Model {
  import scala.collection.mutable.ListBuffer
  val randcVars = new ListBuffer[Randc]
}
