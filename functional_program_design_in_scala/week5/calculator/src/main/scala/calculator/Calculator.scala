package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map (pair =>
      pair._1 -> Signal(eval(getReferenceExpr(pair._1, namedExpressions), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    val ns: List[String] = List()
    def loop(expr: Expr, references: Map[String, Signal[Expr]], names: List[String]): Double = {
      expr match {
        case Literal(v) => v
        case Ref(name) =>
          if (!names.contains(name))
            loop(getReferenceExpr(name, references), references, name :: names)
          else
            Double.NaN
        case Plus(a, b) => loop(a, references, names) + loop(b, references, names)
        case Minus(a, b) => loop(a, references, names) - loop(b, references, names)
        case Times(a, b) => loop(a, references, names) * loop(b, references, names)
        case Divide(a, b) => loop(a, references, names) / loop(b, references, names)
      }
    }
    loop(expr, references, ns)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
