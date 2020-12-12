package crv.backends.jacop

class RandArr(val min: Int, val max: Int, implicit val upper: Model) extends RandObj(upper) {
  var size: Rand = new Rand("size")
  var sum:  Rand = new Rand("sum")
  println(_model)
}
