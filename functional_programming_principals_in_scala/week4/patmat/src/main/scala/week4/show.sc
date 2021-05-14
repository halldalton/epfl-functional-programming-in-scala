import week4.Expr
import week4.Number
import week4.Sum
import week4.Var
import week4.Prod

object exprs {
  def show(e: Expr): String = {
    def brack(e: Expr): String = e match {
      case Sum(l, r) => "(" + show(l) + "+" + show(r) + ")"
      case _ => show(e)
    }
    e match {
      case Number(x) => x.toString
      case Sum(l, r) => show(l) + "+" + show(r)
      case Var(x) => x
      case Prod(l, r) => brack(l) + "*" + brack(r)
    }
  }
}

import exprs.show

show(Number(1))
show(Number(1))
show(Sum(Number(2), Number(3)))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))