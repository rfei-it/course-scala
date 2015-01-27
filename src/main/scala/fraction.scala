package sandbox

class Fraction(n: Int, d: Int) {
  if (d == 0)
    throw new IllegalArgumentException("Denominator cannot equal zero.")

  // Signs are corrected

  val numerator = if (d < 0) -n else n
  val denominator = math.abs(d)

  // Decimal form

  val decimal = numerator.toDouble / denominator

  // Greatest common divisor (Euclidean algorithm)

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else if (a < 0) gcd(-a, b)
    else if (b < 0) gcd(a, -b)
    else if (b > a) gcd(b, a)
    else gcd(b, a % b)

  val gcd: Int = gcd(numerator, denominator)

  // Reduction

  def isReduced = gcd == 1

  def reduced: Fraction =
    if (isReduced) this
    else new Fraction(numerator / gcd, denominator / gcd)

  // Improper fractions

  def isImproper = math.abs(numerator) >= math.abs(denominator)

  def integer: Int = numerator / denominator

  def remainder: Fraction = new Fraction(numerator % denominator, denominator)

  // Inversed

  def inversed = new Fraction(denominator, numerator)

  // Absolute value

  def abs = new Fraction(math.abs(numerator), denominator)

  // Arithmetic operations

  def +(that: Fraction) =
    new Fraction(
      this.numerator * that.denominator +
        this.denominator * that.numerator,
      this.denominator * that.denominator)
      .reduced

  def -(that: Fraction) = this + (-that)

  def *(that: Fraction) =
    new Fraction(
      this.numerator * that.numerator,
      this.denominator * that.denominator)
      .reduced

  def /(that: Fraction) = this * that.inversed

  // Unary operations

  def unary_+ = new Fraction(numerator, denominator)

  def unary_- = new Fraction(-numerator, denominator)

  // Comparison

  override def equals(obj: Any) = obj match {
    case that: Fraction =>
      this.decimal == that.decimal
    case that: Double =>
      this.decimal == that
    case _ => false
  }

  override def hashCode = this.decimal.hashCode

  def <(that: Fraction) = this.decimal < that.decimal
  def <=(that: Fraction) = (this < that) || (this == that)
  def >(that: Fraction) = !(this <= that)
  def >=(that: Fraction) = !(this < that)

  // Printing

  override def toString = {
    var result = numerator.toString
    if (denominator != 1)
      result += "/" + denominator
    result
  }

  def toImproperString =
    if (!isImproper) toString
    else {
      var result = ""
      val i = integer
      val r = remainder
      result += i
      if (r.numerator != 0) {
        if (i > 0)
          result += " + "
        else result += " - "
        result += r.abs.toString
      }
      result
    }

}
