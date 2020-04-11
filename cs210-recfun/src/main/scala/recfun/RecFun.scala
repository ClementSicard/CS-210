package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if r < 0 || c < 0 then throw new IllegalArgumentException else
    if c == 0 || c == r then 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
    
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def compteur(chars: List[Char], c: Int): Int =
      if chars.isEmpty then c else {
        if chars.head == '(' then compteur(chars.tail, c + 1) 
        else if chars.head == ')' then {
            if c >= 1 then compteur(chars.tail, c - 1) else -1
          } 
        else compteur(chars.tail, c)
      }
    
    if compteur(chars, 0) == 0 then true else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def iter(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0 
      else iter(money - coins.head, coins) + iter(money, coins.tail)
    
    iter(money, coins)
  }
}