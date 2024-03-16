import EResource.Table

sealed trait QueryAstToken

case object Selector extends QueryAstToken
case object Pointer extends QueryAstToken
case object Equality extends QueryAstToken
case object Separator extends QueryAstToken
case class Resource(value: EResource) extends QueryAstToken
case class Identifier(value: String) extends QueryAstToken
case class StringLiteral(value: String) extends QueryAstToken

enum EResource:
  case Table(name: String)

object Lexer:
  private def getToken(s: String): Option[QueryAstToken] = s match
    case "S" => Some(Selector)
    case "=>" => Some(Pointer)
    case ":" => Some(Separator)
    case "=" => Some(Equality)
    case _ => Some(Identifier(s))

  def tokenize(input: String): List[QueryAstToken] =
    val chars = input.iterator
    chars.collect {
      case c@'S' => getToken(c.toString)
      case '=' =>
        if (chars.hasNext && chars.next() == '>') getToken("=>")
        else getToken("=")
      case c@'T' =>
        if (chars.hasNext && chars.next() == '(')
          val table = chars.takeWhile(_ != ')').mkString
          Some(Resource(EResource.Table(table)))
        else None
      case c@':' => getToken(c.toString)
      case ' ' | '\t' | '\n' => None
      case '"' =>
        val stringLiteral = chars.takeWhile(_ != '"').mkString
        getToken(stringLiteral)
      case c => getToken(c.toString)
    }.flatten.toList

def mapToSql(input: QueryAstToken): String =
  input match
    case Selector => "SELECT"
    case Pointer => "* FROM"
    case Resource(Table(v)) => v
    case Separator => "WHERE"
    case Equality => "="
    case Identifier(v) => v
    case _ => ""

@main
def main(): Unit = {
  val tokens = Lexer.tokenize("S => T(Users) : \"foo\" = \"bar\"\n")
  println(tokens)
  val sqlList = tokens.map { t => mapToSql(t) }
  println(sqlList.mkString(" "))
}