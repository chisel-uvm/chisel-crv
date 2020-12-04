package crv

/**
  * Helper trait that can contain implicit conversion or other helper utility inside the test environment.
  * It was design to be mixed in with the current test class, for example:
  * {{{
  *  class TestRand extends FlatSpec with VerificationContext
  * }}}
  */
trait VerificationContext {}
