package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal[Double](scala.math.pow(b(),2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal[Set[Double]]( {
      if (delta() > 0) Set[Double]((-b() + scala.math.sqrt(delta()))/(2*a()), (-b() - scala.math.sqrt(delta()))/(2*a()) )
      else if (delta() == 0) Set[Double]( -b()/(2 * a()))
      else Set[Double]()
    })
  }
}
