package net.almost_done.trees

/**
 * Created by nietaki on 3/6/14.
 */
case object ContinuousInteger extends AttributeType {
  type ValueType = Int

  override def retrieveRepresentation(value: ContinuousInteger.ValueType): String = value.toString

  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case repr => {
      Some(repr.toInt) //yes, throwing here
    }
  }
}
