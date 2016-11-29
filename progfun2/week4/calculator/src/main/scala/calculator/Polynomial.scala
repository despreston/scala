package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    //Δ = b² - 4ac
    Signal[Double]((b()*b())-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal[Set[Double]](
      //(-b ± √Δ) / 2a
      if (delta() < 0) {
        Set.empty
      } else {
        val plus: Double = b() * (-1.0) + math.sqrt(delta()) / (2.0 * a())
        val minus: Double = b() * (-1.0) - math.sqrt(delta()) / (2.0 * a())
        Set(plus, minus)
      }
    )
  }
}
