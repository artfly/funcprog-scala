package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    val delta = (a: Double, b: Double, c: Double) => b * b - 4 * a * c
    Signal(delta(a(), b(), c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def solutions(a: Double, b: Double, delta: Double): Set[Double] = {
      if (a == 0 || delta < 0) Set()
      else if (delta == 0) Set(-b / (2 * a))
      else {
        val root = Math.sqrt(delta)
        Set((-b + root) / (2 * a), (-b - root) / (2 * a))
      }
    }

    Signal(solutions(a(), b(), delta()))
  }
}
