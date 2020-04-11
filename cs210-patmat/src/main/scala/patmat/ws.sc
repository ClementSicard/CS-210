def times(chars: List[Char]): List[(Char, Int)] =
    def rec(acc: Map[Char, Int], c: Char) =
      acc +  ((c, acc.get(c).getOrElse(0) + 1))
    (Map[Char, Int]() /: chars)(rec).iterator.toList

times("clement".toList)