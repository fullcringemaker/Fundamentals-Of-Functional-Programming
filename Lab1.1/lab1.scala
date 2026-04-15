val collect: (Int, List[Int]) => (List[Int], List[Int]) = {
  case (x, Nil) => (x :: Nil, Nil)

  case (x, y :: ys) if (y == x) =>
    val (grp, tail) = collect(x, ys)
    (x :: grp, tail)

  case (x, tail) =>
    (x :: Nil, tail)
}

val pack: List[Int] => List[List[Int]] = {
  case Nil => Nil

  case x :: xs =>
    val (grp, tail) = collect(x, xs)
    grp :: pack(tail)
}

/*
pack(List(5,5,5))
pack(List(1,1,2,2,2,1))
pack(List(1,2,1,2))
pack(Nil)
*/