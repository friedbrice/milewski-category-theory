object Challenges2 extends App {

  import java.nio.file.{Files, Paths}
  import scala.collection.mutable
  import scala.collection.concurrent.TrieMap
  import scala.sys.process._

  /* 1. Define a higher-order function (or a function object) `memoize`
     in your favorite language. This function takes a pure function `f`
     as an argument and returns a function that behaves almost exactly
     like `f`, except that it only calls the original function once for
     every argument, stores the result internally, and subsequently
     returns this stored result every time it's called with the same
     argument. You can tell the memoized function from the original by
     watching its performance. For instance, try to memoize a function
     that takes a long time to evaluate. You'll have to wait for the
     result the first time you call it, but on subsequent calls, with
     the same argument, you should get the result immediately. */
  def memoize[A, B](f: A => B): A => B = new Function[A, B] {

    private val store: mutable.Map[A, B] = new TrieMap()

    def apply(x: A): B = store.get(x) match {
      case Some(y) => y
      case None => { val y = f(x); store.+=((x, y)); y }
    }
  }

  /* 2. Try to memoize a function from your standard library that you
     normally use to produce random numbers. Does it work? */
  val memoNextInt: Unit => Int = memoize(_ => util.Random.nextInt)
  val answer2: String =
    """It doesn't really work, because the actual input to `nextInt` is
      | hidden from the caller. From the caller's point of view, it
      | takes a unit, so there is only one valid input, so the memoizer
      | just caches the first value returned.
      |""".stripMargin

  /* 3. Most random number generators can be initialized with a seed.
     Implement a function that takes a seed, calls the random number
     generator with that seed, and returns the result. Memoize that
     function. Does it work? */
  def randInt(seed: Int): Int = (new util.Random(seed)).nextInt()
  val memoRandInt: Int => Int = memoize(randInt)
  val answer3: String = "Seems to work."

  /* 4. Which of these C++ functions are pure? Try to memoize them and
     observe what happens when you call them multiple times: memoized
     and not.

       1. The factorial function from the example in the text.

       2. `std::getchar()`

       3. `bool f() {
               std::cout << "Hello!" << std::endl;
               return true;
           }`

       4. `int f(int x)
           {
               static int y = 0;
               y += x;
               return y;
           }`
  */
  val answer4_1: String = "`factorial` is pure."
  val answer4_2: String =
    "`std::getchar` is not pure, since it reads from stdin."
  val answer4_3: String =
    "`f` is not pure, since it prints to stdout."
  val answer4_4: String =
    """`f` is not pure, since `y` saves state between calls, and that
      | saved state influences the return value.
      |""".stripMargin

  /* 5. How many different functions are there from `Bool` to `Bool`?
     Can you implement them all? */
  def bb1(p: Boolean): Boolean = p
  def bb2(p: Boolean): Boolean = !p
  def bb3(p: Boolean): Boolean = false
  def bb4(p: Boolean): Boolean = true

  /* 6. Draw a picture of a category whose only objects are the types
     `Void`, `()` (unit), and `Bool`; with arrows corresponding to all
     possible functions between these types. Label the arrows with the
     names of the functions. */
  def bu(p: Boolean): Unit = ()
  def uu(u: Unit): Unit = ()
  def ub1(u: Unit): Boolean = false
  def ub2(u: Unit): Boolean = true
  def vv(v: Nothing): Nothing = vv(v)
  def vu(v: Nothing): Unit = vu(v)
  def vb(v: Nothing): Boolean = vb(v)

  val picture: String =
    "\\documentclass{standalone}\n" +
    "\\usepackage[utf8]{inputenc}\n" +
    "\\usepackage[all]{xy}\n" +
    "\\begin{document}\n" +
    "$$\\xymatrix{\n" +
    "  Void \\ar@(u, l)_{vv} \\ar[rr]|-{vu} \\ar[dr]|-{vb} & &\n" +
    "  Unit \\ar@(r, u)_{uu} \\ar@/^18pt/[dl]|-{ub2} \\ar@/^9pt/[dl]|-{ub1} \\\\ &\n" +
    "  Bool \\ar@/^6pt/[ur]|-{bu} \\ar@(ul, dl)_{bb1} \\ar@(l, d)_{bb2} \\ar@(dl, dr)_{bb3} \\ar@(d, r)_{bb4}\n" +
    "}$$\n" +
    "\\end{document}\n"

  def runLatex(latex: String, jobname: String): Unit = {
    s"pdflatex -interaction batchmode -jobname $jobname $latex" !;
    Files.deleteIfExists(Paths.get(s"$jobname.aux"))
    Files.deleteIfExists(Paths.get(s"$jobname.log"))
    open(s"$jobname.pdf")
  }

  def open(str: String): Unit =
    System.getProperty("os.name").toLowerCase match {
      case os if os contains "linux"   => s"xdg-open $str" !
      case os if os contains "mac"     => s"open $str" !
      case os if os contains "windows" => s"start $str" !
    }

  runLatex(picture, "challenges2-6")
}
