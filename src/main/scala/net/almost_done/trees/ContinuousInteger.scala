package net.almost_done.trees

/**
 * Created by nietaki on 3/6/14.
 */
case object ContinuousInteger extends AttributeType {
  type ValueType = Int

  override def retrieveStringRepresentation(value: ContinuousInteger.ValueType): String = value.toString

  override def parseStringRepresentation(representation: String): Option[ContinuousInteger.ValueType] =  {
    var ret: Option[ValueType] = None;
    try {
      Some(representation.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }
}
