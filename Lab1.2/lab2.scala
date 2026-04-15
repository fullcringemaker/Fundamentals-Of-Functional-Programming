class BoolFormula private (f: Map[String, Boolean] => Boolean) {

  def this(name: String) = this(env => env(name))

  def eval(env: Map[String, Boolean]): Boolean = f(env)

  def + (g: BoolFormula): BoolFormula =
    new BoolFormula(env => this.eval(env) || g.eval(env))

  def * (g: BoolFormula): BoolFormula =
    new BoolFormula(env => this.eval(env) && g.eval(env))

  def unary_! : BoolFormula =
    new BoolFormula(env => !this.eval(env))
}

/*
object Main {
  def main(args: Array[String]): Unit = {
    val a = new BoolFormula("a")
    val b = new BoolFormula("b")
    val c = new BoolFormula("c")
    val e = Map("a" -> true, "b" -> false, "c" -> true)
    val form = a * b + c
    println(form.eval(e))
  }
}
*/