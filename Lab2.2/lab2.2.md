% Лабораторная работа № 4 «Case-классы и сопоставление с образцом в Scala»
% 15 апреля 2026 г.
% Дмитрий Трофименко, ИУ9-62Б

# Цель работы
Целью данной работы является приобретение навыков разработки case-классов на языке Scala для 
представления абстрактных синтаксических деревьев.

# Индивидуальный вариант
Абстрактный синтаксис параметризованных выражений:

`Expr → C(Expr, …, Expr) | VARIABLE | NUMBER`
Здесь `C(Expr, …, Expr)` — конструктор данных. Список операндов конструктора может быть пустым.

Примеры выражений:

- `Cons(1, Cons(2, Cons(3, Nil())))` — список из трёх чисел 1, 2, 3, конструктор Cons имееет два 
операнда, конструктор Nil — ноль операндов.
- `Cons(X, Cons(Y, Nil()))` — список из двух звеньев, головы списков неизвестные — заданы переменными.
- `Tree(Leaf(), 1, Tree(Leaf(), 2, Leaf()))` — конструктор Tree с тремя операндами, конструктор Leaf — 
без операндов.
Унификация двух параметризованных выражений e1 и e2 — поиск таких подстановок σ1 и σ2, что e1/σ1 = e2/σ2 
— подстановки переводят их в одно и то же выражение.

Если в выражениях e1 и e2 нет одноимённых переменных, т.е. vars(e1) ∩ vars(e2) = ∅, то можно искать 
общую унифицирующую подстановку σ, такую, что e1/σ = e2/σ.

Требуется написать функцию

`unify : (Expr, Expr) => Option(Map[String, Expr])`
которая ищет унифицирующую подстановку. Можно считать, что повторяющихся переменных в выражениях нет и 
одна и та же переменная не может входить одновременно в оба выражения.

Функция должна возвращать `Some`, если унифицирующая подстановка существует, и `None`, если подстановку 
найти невозможно.

# Реализация

```scala
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
                val newSigma = sigma.map { case (name, expr) => name -> substitute(expr, one) } + 
(x -> term)
                loop(newRest, newSigma)
              }

            case (term, Variable(x)) =>
              if (occurs(x, term)) None
              else {
                val one = Map(x -> term)
                val newRest = rest.map { case (a, b) => (substitute(a, one), substitute(b, one)) }
                val newSigma = sigma.map { case (name, expr) => name -> substitute(expr, one) } + 
(x -> term)
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
```

# Тестирование

```scala
Тест 1
e1 = Constructor(Cons,List(Number(1), Constructor(Cons,List(Number(2), Constructor(Cons,List(Number(3), 
Constructor(Nil,List())))))))
e2 = Constructor(Cons,List(Variable(X), Constructor(Cons,List(Variable(Y), Constructor(Cons,List
(Variable(Z), Constructor(Nil,List())))))))
unify = Some(Map(X -> Number(1), Y -> Number(2), Z -> Number(3)))

Тест 2
e1 = Constructor(Cons,List(Variable(X), Constructor(Cons,List(Variable(Y), Constructor(Nil,List())))))
e2 = Constructor(Cons,List(Number(1), Constructor(Cons,List(Number(2), Constructor(Nil,List())))))
unify = Some(Map(X -> Number(1), Y -> Number(2)))

Тест 3
e1 = Constructor(Tree,List(Constructor(Leaf,List()), Number(1), Constructor(Tree,List(Constructor(Leaf,
List()), Number(2), Constructor(Leaf,List())))))
e2 = Constructor(Tree,List(Variable(A), Variable(B), Variable(C)))
unify = Some(Map(A -> Constructor(Leaf,List()), B -> Number(1), C -> Constructor(Tree,List(Constructor
(Leaf,List()), Number(2), Constructor(Leaf,List())))))

Тест 4
e1 = Constructor(Cons,List(Number(1), Constructor(Nil,List())))
e2 = Constructor(Cons,List(Number(1), Constructor(Cons,List(Number(2), Constructor(Nil,List())))))
unify = None

Тест 5
e1 = Constructor(Tree,List(Constructor(Leaf,List()), Number(1), Constructor(Leaf,List())))
e2 = Constructor(Cons,List(Constructor(Leaf,List()), Number(1), Constructor(Leaf,List())))
unify = None

Тест 6
e1 = Variable(X)
e2 = Constructor(Cons,List(Number(1), Variable(X)))
unify = None
```

# Вывод
В данной лабораторной работе было выполнено описание абстрактного синтаксического дерева для 
параметризованных выражений и разработана функция преобразования, позволяющая решать задачу унификации 
двух выражений. По заданию требовалось находить такую подстановку для переменных, при которой оба 
выражения становятся одинаковыми, и возвращать результат только при существовании корректной 
унифицирующей подстановки.

В ходе работы была реализована обработка выражений на основе сопоставления с образцом и 
последовательного разбора структуры термов. Для вычисления подстановки использовалось поэтапное 
согласование пар подвыражений с накоплением соответствий для переменных и применением этих соответствий 
ко всем оставшимся частям. Также была учтена проверка на появление переменной внутри подставляемого 
выражения, что предотвращает построение некорректных самоссылочных зависимостей и обеспечивает 
корректность результата.

В результате получено решение, которое корректно работает для выражений с конструкторами, числовыми 
значениями и переменными, и возвращает как найденную подстановку, так и отсутствие решения в 
несогласуемых случаях. Тестирование демонстрирует успешное построение подстановок для совместимых 
выражений и корректный отказ при различии структуры, несовпадении конструкторов или попытках получить 
недопустимую подстановку.
