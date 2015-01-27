package sandbox

sealed trait Complex {

  def re: Double

  def im: Double

  def modulus: Double

  def argument: Double

  // Arithmetics

  def +(that: Complex): Complex =
    new RectComplex(this.re + that.re, this.im + that.im)

  def -(that: Complex): Complex =
    new RectComplex(this.re - that.re, this.im - that.im)

  def *(that: Complex): Complex =
    new PolarComplex(this.modulus * that.modulus,
      this.argument + that.argument)

  def /(that: Complex): Complex = {
    require(that != 0)
    new PolarComplex(this.modulus / that.modulus,
      this.argument - that.argument)
  }

  // Comparison

  def tolerance: Double = 0.000001

  override def equals(obj: Any) = obj match {
    case that: Complex =>
      math.abs(this.re - that.re) < tolerance &&
        math.abs(this.im - that.im) < tolerance
    case _ =>
      try {
        math.abs(this.re - obj.toString.toDouble) < tolerance &&
          this.im == 0
      } catch {
        case e: NumberFormatException => false
      }
  }

  override def hashCode = re.hashCode * im.hashCode

  // Printing

  def realPart: String =
    if (re == 0) ""
    else re.toString

  def imaginaryPart: String = {
    var result = ""
    if (im > 0 && re != 0) result += " + "
    if (im < 0) result += " - "
    if (im != 0) {
      result += math.abs(im)
      result += "i"
    }
    result
  }

  override def toString = realPart + imaginaryPart

}

final class RectComplex(val re: Double,
                        val im: Double)
  extends Complex {

  val modulus = math.sqrt(re * re + im * im)
  val argument = math.atan2(im, re)

}

final class PolarComplex(val modulus: Double,
                         val argument: Double)
  extends Complex {

  val re = modulus * math.cos(argument)
  val im = modulus * math.sin(argument)

}
