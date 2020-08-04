package net.nightwhistler.redbook

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

object JSONParsing {

  import JSON._

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val jstringParser = regex("\".*\"".r) map (s => JString(s))
    val jnumParser = regex("[0-9]+(\\.[0-9]+)?".r) map (s => JNumber(s.toDouble))
    val jbool = (string("true") | string("false")) map (s => JBool(s.toBoolean))
    val jnull = string("null") map (_ => JNull)
    val jarray = surround('[', (jsonParser(P) ** char(',')).many, ']') map (s => JArray(s.map(_._1).toIndexedSeq))
    val jobject = (char('{') ** (regex("[A-Za-z0-9]+".r) ** char(':') ** jsonParser(P)).many ** char('}')).map {
      case (((_, pairs), _)) => JObject(pairs.map(p => p._1._1 -> p._2).toMap)
    }

    jstringParser | jnumParser | jbool | jnull | jarray | jobject
  }

}
