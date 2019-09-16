package utils

/**
 * SQLクエリーの組立に使用するユーティリティー
 */
object QueryUtils {

  /**
   * IN句とそのパラメータを作成する
   */
  def toInClause[T](name: String, params: List[T]): (String, List[(Symbol, T)]) = {
    val namesAndParams = params.zipWithIndex.reverse.map {
      case (param, index) => {
        val indexedName = name + index
        (indexedName, Symbol(indexedName) -> param)
      }
    }.foldLeft(List[String](), List[(Symbol, T)]()) {
      case ((names, paramTouples), (n, p)) => (n :: names, p :: paramTouples)
    }

    (namesAndParams._1.mkString(name + " IN ({", "},{", "})"), namesAndParams._2)
  }
}