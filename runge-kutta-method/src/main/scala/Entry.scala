import java.beans.BeanProperty
import java.io.{File, FileInputStream}
import java.util

import breeze.interpolation.CubicInterpolator
import breeze.linalg.{DenseVector, linspace}
import breeze.plot.{Figure, Plot, plot, scatter}
import javax.script._
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.jdk.CollectionConverters._

object Entry extends App {
  val options = CliParser.parse(args)

  val source = options(CliParser.Source).toString
  val destination = options(CliParser.Destination).toString

  /**
   * Загружает YAML-данные из `filename` в формате `Config`
   *
   * @param filename Относительный путь к файлу
   * @return Объект с загруженными данными
   */
  def loadData(filename: String) = {
    val input = new FileInputStream(new File(filename))
    val yaml = new Yaml(new Constructor(classOf[Config]))

    yaml.load(input).asInstanceOf[Config]
  }

  /**
   * Оборачивает правую часть системы ДУ в набор Scala-функций.
   *
   * @param f Определение функций на JavaScript
   * @return Список функций f(x,u)
   */
  def wrapFunctions(f: util.ArrayList[String]): List[RungeKuttaMethod.RightSide] = {
    val engine = new ScriptEngineManager().getEngineByName("nashorn")

    f.toArray.map {
      v => {
        (x: Double, u: Vector[Double]) =>
          engine.eval(
            v.toString, {
              val bindings = new SimpleBindings()

              bindings.put("x", x)
              bindings.put("u", u.toArray)

              bindings
            }).asInstanceOf[Double]
      }
    }.toList
  }

  /**
   * Рисует графики решения
   *
   * @param p      График, на который происходит отображение
   * @param values Значения функций в опорных точках
   */
  def draw(p: Plot, values: List[(Double, Vector[Double])]): Unit = {
    val xValues = new DenseVector(values.map(_._1).toArray)

    val interpolations = u0.indices.map {
      i => CubicInterpolator(xValues, new DenseVector(values.map(_._2(i)).toArray))
    }

    val points = u0.indices.map {
      i => new DenseVector(values.map(_._2(i)).toArray)
    }

    interpolations.zipWithIndex.foreach { case (v, i) =>
      p += plot(x, x.map(v(_)), name = s"u${i + 1}(x)")
    }

    points.zipWithIndex.foreach { case (yValues, i) =>
      p += scatter(xValues, yValues, _ => (config.b - config.a) / 200, name = s"опорные точки u${i + 1}(x)")
    }
  }

  /**
   * Рисует графики точных решений
   *
   * @param p         График, на который происходит отображение
   * @param functions Функции вида u = f(x)
   */
  def drawExact(p: Plot, functions: List[RungeKuttaMethod.RightSide]): Unit = {
    functions.zipWithIndex.foreach { case (fn, i) =>
      p += plot(x, x.map(fn(_, Vector())), name = s"точная u${i + 1}(x)")
    }
  }

  val config = loadData(source)

  val u0 = config.u0.asScala.toVector.map(_.toDouble)

  val f = wrapFunctions(config.f)
  val exact = config.exact match {
    case null => List()
    case fn => wrapFunctions(fn)
  }

  val (rk, rkf) = {
    val method = new RungeKuttaMethod(f, config.x0, u0)

    (method.RK4(config.step, config.b), method.RKF24(config.step, config.b, config.eps))
  }

  val figure = Figure()
  val x = linspace(config.a, config.b)

  // График решения с постоянным шагом
  val p = figure.subplot(0)
  p.title = s"Постоянный шаг, h = ${config.step}"
  p.xlabel = "x axis"
  p.ylabel = "y axis"
  p.legend = true

  draw(p, rk)
  drawExact(p, exact)

  val p2 = figure.subplot(2, 1, 1)
  p2.title = s"Автоматический выбор шага (вложенный метод), eps = ${config.eps}, шагов: ${rkf.length}"
  p2.xlabel = "x axis"
  p2.ylabel = "y axis"
  p2.legend = true

  draw(p2, rkf)
  drawExact(p2, exact)

  if (config.exact != null) {
    val norm: List[(Double, Vector[Double])] => Double = v => v.map {
      case (x, u) => exact.map(fn => fn(x, Vector())).zip(u).map(v => Math.abs(v._1 - v._2)).max
    }.max

    val rkNorm = norm(rk)
    val rkfNorm = norm(rkf)

    p.title += s"\nНорма глобальной погрешности: $rkNorm"
    p2.title += s"\nНорма глобальной погрешности: $rkfNorm"
  }

  figure.width = 1024
  figure.height = 1280
  figure.saveas(destination)
}

class Config {
  @BeanProperty var x0: Double = 0d
  @BeanProperty var u0: util.ArrayList[String] = new util.ArrayList[String]()
  @BeanProperty var f: util.ArrayList[String] = new util.ArrayList[String]()
  @BeanProperty var a: Double = 0d
  @BeanProperty var b: Double = 0d
  @BeanProperty var step: Double = 1d
  @BeanProperty var eps: Double = 1d
  @BeanProperty var exact: util.ArrayList[String] = _
}
