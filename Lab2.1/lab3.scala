class Matrix[T](val rows: List[List[T]]) {
  val size: Int = rows.length

  def get(i: Int, j: Int): T = rows(i)(j)

  def transpose: Matrix[T] = {
    var result = List[List[T]]()
    var j = 0
    while (j < size) {
      var newRow = List[T]()
      var i = 0
      while (i < size) {
        newRow = newRow :+ rows(i)(j)
        i += 1
      }
      result = result :+ newRow
      j += 1
    }
    new Matrix[T](result)
  }

  def square(implicit num: Numeric[T]): Matrix[T] = {
    var result = List[List[T]]()
    var i = 0
    while (i < size) {
      var newRow = List[T]()
      var j = 0
      while (j < size) {
        var sum = num.zero
        var k = 0
        while (k < size) {
          sum = num.plus(sum, num.times(rows(i)(k), rows(k)(j)))
          k += 1
        }
        newRow = newRow :+ sum
        j += 1
      }
      result = result :+ newRow
      i += 1
    }
    new Matrix[T](result)
  }

  override def toString: String = {
    var s = ""
    var i = 0
    while (i < size) {
      s += "["
      var j = 0
      while (j < size) {
        s += rows(i)(j).toString
        if (j != size - 1) {
          s += ", "
        }
        j += 1
      }
      s += "]"
      if (i != size - 1) {
        s += "\n"
      }
      i += 1
    }
    s
  }
}

object Main extends App {
  val m1 = new Matrix[Int](
    List(
      List(1, 2),
      List(3, 4)
    )
  )
  println(m1)
  println(m1.get(0, 1))
  println(m1.transpose)
  println(m1.square)
  println()

  val m2 = new Matrix[Double](
    List(
      List(1.0, 2.0),
      List(3.0, 4.0)
    )
  )

  println(m2)
  println(m2.get(1, 0))
  println(m2.transpose)
  println(m2.square)
  println()

  val m3 = new Matrix[String](
    List(
      List("a", "b"),
      List("c", "d")
    )
  )
  println(m3)
  println(m3.get(1, 1))
  println(m3.transpose)
  // println(m3.square)
}