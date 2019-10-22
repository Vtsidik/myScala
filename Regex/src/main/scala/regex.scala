import scala.util.parsing.combinator.RegexParsers
import collection.mutable
import scala.io.Source._
import java.io._

abstract class Exp
case class Lit(c: Char) extends Exp
case class Or(left: Exp, right: Exp) extends Exp
case class Rep(exp: Exp) extends Exp
case class Conc(first: Exp, second: Exp) extends Exp

abstract class State
class Consume(val c: Char, val out: State) extends State
class Split(val out1: State, val out2: State) extends State
class Placeholder(var pointingTo: State) extends State
case class Match() extends State

object Regex extends RegexParsers
{
  def low: Parser[Exp] = repeat | lit
  def mid: Parser[Exp] = concat | low
  def high: Parser[Exp] = or | mid

  def ch: Parser[Exp] = ("""\w""".r | ".") ^^
    {
      char => assert(char.length == 1); Lit(char.head)
    }
  def ex: Parser[Exp] = "(" ~> high <~ ")"
  def lit: Parser[Exp] = ch | ex

  def repeat: Parser[Exp] = lit <~ "*" ^^
    {
      case l => Rep(l)
    }
  def concat: Parser[Exp] = rep(low) ^^
    {
      case list => listToConcat(list)
    }
  def or: Parser[Exp] = mid ~ "|" ~ mid ^^
    {
      case l ~ "|" ~ r => Or(l, r)
    }

  def listToConcat(l: List[Exp]): Exp = l match
  {
    case head :: Nil => head
    case head :: rest => Conc(head, listToConcat(rest))
  }

  def apply(input: String): Option[Exp] = parseAll(high, input) match
  {
    case Success(result, _) => Some(result)
    case failure : NoSuccess => None
  }
}

object NFA
{
  def regexToNFA(regex: Exp): State = regexToNFA(regex, Match())
  private def regexToNFA(regex: Exp, andThen: State): State =
  {
    regex match
    {
      case Lit(c) => new Consume(c, andThen)
      case Conc(first, second) =>
      {
        regexToNFA(first, regexToNFA(second, andThen))
      }
      case Or(l, r) => new Split(regexToNFA(l, andThen),regexToNFA(r, andThen))
      case Rep(r) =>
        val placeholder = new Placeholder(null)
        val split = new Split(regexToNFA(r, placeholder),andThen)
        placeholder.pointingTo = split
        placeholder
    }
  }
}

object NFAEvaluator
{
  def evaluate(nfa: State, input: String): Boolean = evaluate(Set(nfa), input)
  def evaluate(nfas: Set[State], input: String): Boolean =
  {
    input match
    {
      case "" => evaluateStates(nfas, None).exists(_ == Match())
      case string => evaluate(evaluateStates(nfas, input.headOption), string.tail)
    }
  }
  def evaluateStates(nfas: Set[State], input: Option[Char]): Set[State] =
  {
    val visitedStates = mutable.Set[State]()
    nfas.flatMap(state => evaluateState(state, input, visitedStates))
  }
  def evaluateState(currentState: State, input: Option[Char], visitedStates: mutable.Set[State]): Set[State] =
  {
    if (visitedStates contains currentState)
    {
      Set()
    }
    else
    {
      visitedStates.add(currentState)
      currentState match
      {
        case placeholder: Placeholder => evaluateState(placeholder.pointingTo, input, visitedStates)
        case consume: Consume => if (Some(consume.c) == input || consume.c == '.') Set(consume.out) else Set()
        case s: Split => evaluateState(s.out1, input, visitedStates) ++ evaluateState(s.out2, input, visitedStates)
        case m: Match => if (input.isDefined) Set() else Set(Match())
      }
    }
  }
}

object DoesMatch
{
  def fullMatch(pattern: String, input: String) =
  {
    val parsed = Regex(pattern).getOrElse(throw new RuntimeException("Failed to parse regex"))
    val nfa = NFA.regexToNFA(parsed)
    NFAEvaluator.evaluate(nfa, input)
  }
  def matchAnywhere(pattern: String,input: String) = fullMatch(".*" + pattern + ".*", input)
}

object Run {

  val line = fromFile("C:\\Users\\по\\Desktop\\Regex\\src\\main\\scala\\prim.txt").mkString
  val stringArray = line.split("\\s+")
  var i = 1

  def main(args: Array[String]): Unit =
  {
    val filePath = "C:\\Users\\по\\Desktop\\Regex\\src\\main\\scala\\otvet.txt"
    val writer = new PrintWriter(new File(filePath))
    println(line)
    print(stringArray(0) + " ")
    val str = stringArray(0) + " "
    writer.write(str)
    while (i < stringArray.length)
    {
      val res = DoesMatch.fullMatch(stringArray(0),stringArray(i))
      if (res == false)
      {
        writer.write("false ")
        print("false ")
      } else
      {
        writer.write("true ")
        print("true ")
      }
      i += 1
    }
    writer.close()
  }
}