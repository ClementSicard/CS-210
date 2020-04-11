package newInterpreter

class RecursiveLanguageSuite {
  import org.junit._
  import org.junit.Assert.{assertEquals, fail}
  import RecursiveLanguage._, Expr.{Constant => C, Name => N, _}

  @Test def gcdTests(): Unit = {
    def test(a: Int, b: Int, c: Int): Unit = {
      val call   = Call(Call(N("gcd"), C(a)), C(b))
      val result = eval(call, definitions)
      assertEquals(C(c), result)
    }
    test(60,  90,  30)
    test(36,  48,  12)
    test(25,  85,  5 )
    test(12,  15,  3 )
    test(60,  75,  15)
    test(35,  56,  7 )
    test(96,  128, 32)
    test(120, 135, 15)
    test(150, 225, 75)
  }

  @Test def evalTests: Unit = {
    assertEquals(Empty, eval(Empty, Map()))
    assertEquals(Cons(C(1), Empty), eval(Cons(C(1), Empty), Map()))
    assertEquals(Cons(C(2), Empty), eval(Cons(BinOp(BinOps.Plus, C(1), C(1)), Empty), Map()))

    assertEquals(C(0),  eval(Match(Empty,              C(0), "_", "_",  C(1)),    Map()))
    assertEquals(C(1),  eval(Match(Cons(Empty, Empty), C(0), "_", "_",  C(1)),    Map()))
    assertEquals(C(2),  eval(Match(Cons(C(2), Empty),  C(0), "x", "xs", N("x")),  Map()))
    assertEquals(Empty, eval(Match(Cons(C(2), Empty),  C(0), "x", "xs", N("xs")), Map()))

    try {
      eval(Match(C(0), C(0), "_", "_", C(1)), Map())
      fail()
    } catch {
      case EvalException(msg) => // OK!
    }
  }

  @Test def freeVarsTests: Unit = {
    assertEquals(Set(),         freeVars(Empty))
    assertEquals(Set("a", "b"), freeVars(Cons(N("a"), N("b"))))
    assertEquals(Set(),         freeVars(Match(Empty, C(0),   "a", "b", N("a"))))
    assertEquals(Set(),         freeVars(Match(Empty, C(0),   "a", "b", N("b"))))
    assertEquals(Set("c"),      freeVars(Match(Empty, N("c"), "_", "_", C(1))))
  }

  @Test def alphaConvertTests: Unit = {
    assertEquals(Cons(N("b"), N("b")), alphaConvert(Cons(N("a"), N("a")), "a", "b"))
    assertEquals(Match(Empty, C(0),   "a", "b", N("a")), alphaConvert(Match(Empty, C(0),   "a", "b", N("a")), "a", "b"))
    assertEquals(Match(Empty, C(0),   "a", "b", N("b")), alphaConvert(Match(Empty, C(0),   "a", "b", N("b")), "a", "b"))
    assertEquals(Match(Empty, N("a"), "a", "b", C(1)),   alphaConvert(Match(Empty, N("c"), "a", "b", C(1)),   "c", "a"))
    assertEquals(Match(Empty, N("b"), "a", "b", C(1)),   alphaConvert(Match(Empty, N("c"), "a", "b", C(1)),   "c", "b"))
    assertEquals(Match(Empty, C(0),   "a", "b", N("e")), alphaConvert(Match(Empty, C(0),   "a", "b", N("d")), "d", "e"))
  }

  val sum = "sum" -> Fun("a", Fun("b", BinOp(BinOps.Plus, N("a"), N("b"))))
  val div = "div" -> Fun("a", Fun("b", BinOp(BinOps.DividedBy, N("a"), N("b"))))

  @Test def foldLeft: Unit = {
    val list1 = Cons(C(1), Cons(C(2), Cons(C(3), Empty)))
    val call1 = Call(Call(Call(N("foldLeft"), list1), C(100)), N("sum"))
    assertEquals(C(106), eval(call1, definitions + sum))

    val list2 = Cons(C(1), Cons(C(2), Cons(C(3), Cons(C(4), Cons(C(5), Cons(C(6), Empty))))))
    val call2 = Call(Call(Call(N("foldLeft"), list2), C(100000)), N("div"))
    assertEquals(C(138), eval(call2, definitions + div))
  }

  @Test def foldRight: Unit = {
    val list1 = Cons(C(1), Cons(C(2), Cons(C(3), Empty)))
    val call1 = Call(Call(Call(N("foldRight"), list1), C(100)), N("sum"))
    assertEquals(C(106), eval(call1, definitions + sum))

    val list2 = Cons(C(1000000), Cons(C(20000), Cons(C(3000), Cons(C(400), Cons(C(50), Cons(C(6), Empty))))))
    val call2 = Call(Call(Call(N("foldRight"), list2), C(1)), N("div"))
    assertEquals(C(3003), eval(call2, definitions + div))
  }

  @Test def substitutionSimple: Unit = {
    // Substitution should happend everywhere in the Match. In scrutinee and in caseCons:
    assertEquals(Cons(C(1), Empty), eval(
      Call(N("bar"), Cons(C(1), Empty)),
      Map("bar" -> Fun("x", Match(N("x"), C(0), "y", "z", N("x"))))
    ))

    // In scrutinee and in caseEmpty:
    assertEquals(Empty, eval(
      Call(N("bar"), Empty),
      Map("bar" -> Fun("x", Match(N("x"), N("x"), "y", "z", C(0))))
    ))

    // But not inside caseCons when the binding name clashes with the functions name:
    assertEquals(C(1), eval(
      Call(N("bar"), Cons(C(1), Empty)),
      Map("bar" -> Fun("x", Match(N("x"), C(0), "x", "z", N("x"))))
    ))
  }

  @Test def substitutionFunctionCapture: Unit = {
    // Here comes the real fun, plus_one uses "map" as it's first argument name,
    // incorrect implementation will accidentally capture the recursion in map
    // turn into that name into a reference to first argument of plus_one.
    val plus_one = "plus_one" -> Fun("map", BinOp(BinOps.Plus, C(1), N("map")))
    val list1 = Cons(C(1), Cons(C(2), Cons(C(3), Empty)))
    val list2 = Cons(C(2), Cons(C(3), Cons(C(4), Empty)))
    assertEquals(list2, eval(Call(Call(N("map"), list1), N("plus_one")), definitions + plus_one))
  }

  @Test def substitutionMatchCapture: Unit = {
    // More fun, this uses "fact" as a first binding in the pattern match:
    val pairMap = "pairMap" -> Fun("pair", Fun("function",
      Match(
        N("pair"),
        Empty,
        "fact", "tcaf",
        Cons(Call(N("function"), N("fact")), Call(N("function"), N("tcaf"))))
    ))
    assertEquals(Cons(C(6), C(24)), eval(Call(Call(N("pairMap"), Cons(C(3), C(4))), N("fact")), definitions + pairMap))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(200 * 1000)
}
