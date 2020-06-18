import scala.collection.mutable.ListBuffer

object RungeKuttaMethod {
  /**
   * Правая часть дифференциального уравнения:
   *
   * f = f(x,u)
   */
  type RightSide = (Double, Vector[Double]) => Double
}

class RungeKuttaMethod(fun: List[RungeKuttaMethod.RightSide], x0: Double, u0: Vector[Double]) {
  /**
   * Вычисляет таблицу значений решения системы дифференциальных уравнений
   * методом Рунге-Кутты 4 порядка точности на сетке [x0, xn]/) с шагом h.
   */
  def RK4(h: Double, xn: Double): List[(Double, Vector[Double])] = {
    val grid = Stream.from(0)
      .map { i => x0 + i * h }
      .takeWhile { v => v <= xn }

    val table: ListBuffer[(Double, Vector[Double])] = ListBuffer((x0, u0))

    var u = u0
    for (x <- grid.tail) {
      u = rk(h, x, u)

      table.append((x, u))
    }

    table.toList
  }

  def RKF24(h: Double, xn: Double, eps: Double): List[(Double, Vector[Double])] = {
    val table: ListBuffer[(Double, Vector[Double])] = ListBuffer((x0, u0))

    var x = x0
    var u = u0
    var h = h

    while (x < xn) {
      val xNext = x + h
      val (e, y, _) = egorov(h, x, u)

      val err = Math.sqrt(e.map(v => v * v).sum)
      h = Math.pow(eps / err, 0.2) * h

      if (err <= eps) {
        x = xNext
        u = y

        table.append((x, u))
      }
    }

    table.toList
  }

  //noinspection DuplicatedCode
  private def rk(h: Double, x: Double, u: Vector[Double]): Vector[Double] = {
    val k1 = fun.map { f => f(x, u) * h }
    val k2 = fun.map { f => f(x + h / 2, u.zip(k1).map { case (l, r) => l + r / 2 }) * h }
    val k3 = fun.map { f => f(x + h / 2, u.zip(k2).map { case (l, r) => l + r / 2 }) * h }
    val k4 = fun.map { f => f(x + h, u.zip(k3).map { case (l, r) => l + r }) * h }

    u.zip(k1).zip(k2).zip(k3).zip(k4).map {
      case ((((u, k1), k2), k3), k4) =>
        u + (k1 + 2 * k2 + 2 * k3 + k4) / 6
    }
  }

  /**
   * Вычисляет контрольный член Егорова, y и ~y.
   *
   * @param h Вычислительный шаг
   * @param x x0
   * @param u u0
   * @return Контрольный член Егорова, y, ~y
   */
  //noinspection DuplicatedCode
  private def egorov(h: Double, x: Double, u: Vector[Double]): (Vector[Double], Vector[Double], Vector[Double]) = {
    val k1 = fun.map { f => f(x, u) * h }
    val k2 = fun.map { f => f(x + h / 2, u.zip(k1).map { case (l, r) => l + r / 2 }) * h }
    val k3 = fun.map { f => f(x + h / 2, u.zip(k2).map { case (l, r) => l + r / 2 }) * h }
    val k4 = fun.map { f => f(x + h, u.zip(k3).map { case (l, r) => l + r }) * h }

    val y = u.zip(k1).zip(k2).zip(k3).zip(k4).map {
      case ((((u, k1), k2), k3), k4) =>
        u + (k1 + 2 * k2 + 2 * k3 + k4) / 6
    }

    // TODO: убрать ~y, не используется в вычислениях (?)
    val yTilde = u.zip(k1).zip(k2).zip(k3).zip(k4).map {
      case ((((u, k1), k2), k3), k4) =>
        u + (-k1 + 2 * k2 + 2 * k3 - k4) / 2
    }

    val yEgorov = y.zip(yTilde).map {
      case (a, b) => a - b
    }

    (yEgorov, y, yTilde)
  }
}
