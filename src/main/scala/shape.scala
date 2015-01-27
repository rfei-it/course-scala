package sandbox

abstract class Shape {

  def perimeter: Double

  def area: Double

}

class Circle(val radius: Double) extends Shape {

  def diameter = 2 * radius

  def perimeter = math.Pi * diameter

  def area = math.Pi * radius * radius

}

class Rectangle(val ab: Double,
                val ac: Double)
  extends Shape {

  def cd = ab

  def bd = ac

  def bc = math.sqrt(ab * ab + ac * ac)

  def ad = bc

  def perimeter = 2 * (ab + ac)

  def area = ab * ac

}

class Triangle(val ab: Double,
               val ac: Double,
               val a: Double)
  extends Shape {

  def bc = math.sqrt(
    (ab * ab) + (ac * ac) - 2 * ab * ac * math.cos(a))

  def b = math.atan(ac / ab)

  def c = math.atan(ab / ac)

  def perimeter = ab + ac + bc

  def area = ab * ac * math.sin(a) / 2
}
