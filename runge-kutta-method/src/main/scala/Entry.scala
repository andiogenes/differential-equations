import java.beans.BeanProperty
import java.io.{File, FileInputStream}
import java.util

import breeze.interpolation.CubicInterpolator
import breeze.linalg.DenseVector
import breeze.linalg.linspace
import breeze.plot.Figure
import breeze.plot.plot

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

  val config = loadData(source)

  val u0 = config.u0.asScala.toVector.map(_.toDouble)
  val f = wrapFunctions(config.f)

  val rk = new RungeKuttaMethod(f, config.x0, u0).RK4(config.step, config.b)

  val rkInterpolations = {
    val x = new DenseVector(rk.map(_._1).toArray)

    u0.indices.map {
      i => CubicInterpolator(x, new DenseVector(rk.map(_._2(i)).toArray))
    }
  }

  val figure = Figure()
  val x = linspace(config.a, config.b)

  // График решения с постоянным шагом
  val p = figure.subplot(0)
  p.xlabel = "x axis"
  p.ylabel = "y axis"
  p.legend = true

  rkInterpolations.zipWithIndex.foreach { case (v, i) =>
    p += plot(x, x.map(v(_)), name = s"u${i + 1}(x)")
  }

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
