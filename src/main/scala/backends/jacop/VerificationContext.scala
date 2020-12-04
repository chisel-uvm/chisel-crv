package backends.jacop

trait VerificationContext extends crv.VerificationContext {

  /**
    * Converts a BigInt to [[Rand]].
    *
    * @param i integer to be converted.
    */
  implicit def BigIntToRand(i: BigInt)(implicit model: Model): Rand = {
    require(i < Int.MaxValue)
    new Rand(i.toInt, i.toInt)
  }

  /**
    * Converts a Rand to BigInt.
    *
    * @param r [[Rand]] variable to be converted.
    */
  implicit def RandToBigInt(r: Rand): BigInt = r.value()
}
