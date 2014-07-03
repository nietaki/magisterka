package net.almost_done.data_processing

/**
 * Created by nietaki on 24.06.14.
 */
object DataTransformer {
  /**
   * @param row string containing the data or attribute row
   * @return an array of sanitized (space to underscore and all) values
   */
  def sanitizeAndSplitRow(row: String): Array[String] = {
    val noDot = row.stripSuffix(".") //remove the dots from the end (if neccessary)
    val separators: Array[Char] = ":,".toCharArray //split category (if neccessary) and values
    noDot.split(separators).map{sanitize(_)}
  }

  def sanitize(term: String): String = {
    term.trim().replace(' ', '_') //trim and change spaces to underscores
  }

}
