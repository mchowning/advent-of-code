
object Day6 {

  def getMostCommonChar: (List[Char]) => Char =
    _.groupBy(identity)
      .mapValues(_.size)
      .toList
      .sortBy(_._2)
      .last
      ._1
}
