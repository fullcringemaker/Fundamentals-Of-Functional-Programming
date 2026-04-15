abstract class Expr

case class Constructor(name: String, args: List[Expr]) extends Expr
case class Variable(name: String) extends Expr
case class Number(value: Int) extends Expr

object Main {
  def substitute(expr: Expr, sigma: Map[String, Expr]): Expr = expr match {
    case Variable(name) =>
      sigma.get(name) match {
        case Some(value) => substitute(value, sigma)
        case None => Variable(name)
      }
    case Number(value) => Number(value)
    case Constructor(name, args) =>
      Constructor(name, args.map(arg => substitute(arg, sigma)))
  }

  def occurs(name: String, expr: Expr): Boolean = expr match {
    case Variable(v) => v == name
    case Number(_) => false
    case Constructor(_, args) => args.exists(arg => occurs(name, arg))
  }

  def unify(e1: Expr, e2: Expr): Option[Map[String, Expr]] = {
    def loop(pending: List[(Expr, Expr)], sigma: Map[String, Expr]): Option[Map[String, Expr]] =
      pending match {
        case Nil =>
          Some(sigma.map { case (name, expr) => name -> substitute(expr, sigma) })

        case (left, right) :: rest =>
          val l = substitute(left, sigma)
          val r = substitute(right, sigma)

          (l, r) match {
            case (Variable(x), Variable(y)) if x == y =>
              loop(rest, sigma)

            case (Variable(x), term) =>
              if (occurs(x, term)) None
              else {
                val one = Map(x -> term)
                val newRest = rest.map { case (a, b) => (substitute(a, one), substitute(b, one)) }
                val newSigma = sigma.map { case (name, expr) => name -> substitute(expr, one) } + (x -> term)
                loop(newRest, newSigma)
              }

            case (term, Variable(x)) =>
              if (occurs(x, term)) None
              else {
                val one = Map(x -> term)
                val newRest = rest.map { case (a, b) => (substitute(a, one), substitute(b, one)) }
                val newSigma = sigma.map { case (name, expr) => name -> substitute(expr, one) } + (x -> term)
                loop(newRest, newSigma)
              }

            case (Number(a), Number(b)) if a == b =>
              loop(rest, sigma)

            case (Constructor(name1, args1), Constructor(name2, args2))
                if name1 == name2 && args1.length == args2.length =>
              loop(args1.zip(args2) ::: rest, sigma)

            case _ =>
              None
          }
      }

    loop(List((e1, e2)), Map.empty)
  }

  def c(name: String, args: Expr*): Expr = Constructor(name, args.toList)

  def printTest(title: String, e1: Expr, e2: Expr): Unit = {
    println(title)
    println("e1 = " + e1)
    println("e2 = " + e2)
    println("unify = " + unify(e1, e2))
    println()
  }

  def main(args: Array[String]): Unit = {
    val expr1 = c("Cons", Number(1), c("Cons", Number(2), c("Cons", Number(3), c("Nil"))))
    val expr2 = c("Cons", Variable("X"), c("Cons", Variable("Y"), c("Cons", Variable("Z"), c("Nil"))))

    val expr3 = c("Cons", Variable("X"), c("Cons", Variable("Y"), c("Nil")))
    val expr4 = c("Cons", Number(1), c("Cons", Number(2), c("Nil")))

    val expr5 = c("Tree", c("Leaf"), Number(1), c("Tree", c("Leaf"), Number(2), c("Leaf")))
    val expr6 = c("Tree", Variable("A"), Variable("B"), Variable("C"))

    val expr7 = c("Cons", Number(1), c("Nil"))
    val expr8 = c("Cons", Number(1), c("Cons", Number(2), c("Nil")))

    val expr9 = c("Tree", c("Leaf"), Number(1), c("Leaf"))
    val expr10 = c("Cons", c("Leaf"), Number(1), c("Leaf"))

    val expr11 = Variable("X")
    val expr12 = c("Cons", Number(1), Variable("X"))

    printTest("Тест 1", expr1, expr2)
    printTest("Тест 2", expr3, expr4)
    printTest("Тест 3", expr5, expr6)
    printTest("Тест 4", expr7, expr8)
    printTest("Тест 5", expr9, expr10)
    printTest("Тест 6", expr11, expr12)
  }
}   