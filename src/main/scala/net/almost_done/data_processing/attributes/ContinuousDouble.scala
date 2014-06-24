package net.almost_done.data_processing.attributes

/**
 * Created by nietaki on 3/6/14.
 */
case object ContinuousDouble extends AttributeType  {
  type ValueType = Double

  override def retrieveRepresentation(value: ValueType): String = value.toString

  //TODO catch the exception here and do another partial function that will drop the None, same in Integer
  override def parseRepresentationInner: PartialFunction[String, Option[ValueType]] =  {
    case repr => {
      Some(repr.toDouble) //yes, throwing here
    }
  }

}
