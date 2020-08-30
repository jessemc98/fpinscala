// A comment!
/* Another comment */
/** a documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }
  // 0, 1, 1, 2, 3, 5
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int = 
      if (n <= 0) a
      else go(n -1, b, a + b)

    go(n, 0, 1)
  }
  
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(i: Int): Boolean =
      if (i + 1 >= as.length) true
      else if (ordered(as(i), as(i + 1))) loop(i + 1)
      else false
    loop(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  private def formatResult(fn: Int => Int, name: String, x: Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, x, fn(x))
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1,2,4,3), (x: Int, y: Int) => x <= y))
  }

}