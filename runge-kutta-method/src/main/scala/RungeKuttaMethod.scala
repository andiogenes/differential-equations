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
  //noinspection DuplicatedCode
  def RK4(h: Double, xn: Double): List[(Double, Vector[Double])] = {
    val grid = Stream.from(0)
      .map { i => x0 + i * h }
      .takeWhile { v => v <= xn }

    val table: ListBuffer[(Double, Vector[Double])] = ListBuffer((x0, u0))

    var u = u0
    for (x <- grid.tail) {
      val k1 = fun.map { f => f(x, u) * h }
      val k2 = fun.map { f => f(x + h / 2, u.zip(k1).map { case (l, r) => l + r / 2 }) * h }
      val k3 = fun.map { f => f(x + h / 2, u.zip(k2).map { case (l, r) => l + r / 2 }) * h }
      val k4 = fun.map { f => f(x + h, u.zip(k3).map { case (l, r) => l + r }) * h }

      u = u.zip(k1).zip(k2).zip(k3).zip(k4).map {
        case ((((u, k1), k2), k3), k4) =>
          u + (k1 + 2 * k2 + 2 * k3 + k4) / 6
      }
      table.append((x, u))
    }

    table.toList
  }
}
