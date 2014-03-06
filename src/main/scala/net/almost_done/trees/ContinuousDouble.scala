package net.almost_done.trees

/**
 * Created by nietaki on 3/6/14.
 */
case object ContinuousDouble extends AttributeType  {
  type ValueType = Double

  override def retrieveStringRepresentation(value: ValueType): String = value.toString

  override def parseStringRepresentation(representation: String): Option[ValueType] =  {
    var ret: Option[ValueType] = None;
    try {
      Some(representation.toDouble)
    } catch {
      case _: NumberFormatException => None
    }
  }

}
