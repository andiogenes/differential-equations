object CliParser {
  val usage =
    """
Usage: app [--source filename] [--dest filename]
"""

  val Source: Symbol = Symbol("source")
  val Destination: Symbol = Symbol("dest")

  val defaultOptions = Map(
    Source -> "assignment.yml",
    Destination -> "result.yml"
  )

  type OptionMap = Map[Symbol, Any]

  def parse(args: Array[String]): OptionMap = {
    @scala.annotation.tailrec
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "--source" :: value :: tail =>
          nextOption(map ++ Map(Symbol("source") -> value), tail)
        case "--dest" :: value :: tail =>
          nextOption(map ++ Map(Symbol("dest") -> value), tail)
        case option :: _ =>
          println(s"Unknown option $option")
          map
      }
    }

    nextOption(defaultOptions, args.toList)
  }
}
