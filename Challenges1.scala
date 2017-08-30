object Challenges1 {

  /* 1. Implement, as best as you can, the identity function in your
     favorite language (or the second favorite, if your favorite
     language happens to be Haskell). */
  def id[A]: A => A = (x: A) => x

  /* 2. Implement the composition function in your favorite language. It
     takes two arguments and returns a function that is their
     composition. */
  implicit class ComposeFunction[B, C](g: B => C) {
    def ∘[A](f: A => B): A => C = (x: A) => g(f(x))
  }

  /* 3. Write a program that tries to test that your composition
     function respects identity. */
  val composeTest: Boolean = {

    val cases: List[(Int => Int, Int)] = for {
      x <- (-499 to 500).toList
      f <- List((z: Int) => x + z, (z: Int) => x * z)
    } yield (f, x)

    cases.forall { case (f, x) =>
      (id[Int] ∘ f)(x) == f(x) && (f ∘ id[Int])(x) == f(x)
    }
  }

  /* 4. Is the world-wide web a category in any sense? Are links
     morphisms? */
  val answer4: String =
    """The web is a category with web pages as objects, but the links
      | are not morphisms, since not every page links to itself and if
      | page X is linked to page Y, and page Y is linked to page Z,
      | there's no guarantee that page X links to page Z. Instead of
      | using links as morphisms, the morphisms are sequences of links
      | that you as a surfer could follow in order.
      |""".stripMargin

  /* 5. Is Facebook a category, with people as objects and friendships
     as morphisms? */
  val answer5: String =
    """Facebook has the same problem as the web. I am friends with
      | Jackie, and Jackie is friends with Joe, but I'm not friends with
      | Joe; I hate they guy.
      |""".stripMargin

  /* 6. When is a directed graph a category? */
  val answer6: String =
    """Facebook and the web are both directed graphs. A directed graph
      | is usually not a category with objects being the verticies and
      | morphisms being the edges, since edges don't necessarily compose
      | and verticies don't necessarily have loops, but you can get a
      | category by letting objects be verticies and morphisms be paths
      | on the graph.
      |""".stripMargin
}
