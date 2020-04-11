def tails(ls: List[Int]) : List[List[Int]] = ls :: (ls match {
  case Nil => Nil
  case x :: xs => xs :: tails(xs)
})

tails(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))


def longest[A](ls: List[A]): Int = ls.foldLeft((Option.empty[A], 0, 0)) {
  case ((last, cur, max), x) =>
    val last2 = Some(x)
    val cur2 = if (last2 == last) cur + 1 else 1
    (last2, cur2, if (cur2 > max) cur2 else max)
}._3

longest(List(1,1,1,1,2,2,5,6,1,1,1))