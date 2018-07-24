object ConsoleIO {

  sealed trait ConsoleIO[A] {
    def map[B](f: A => B): ConsoleIO[B] = Map(this, f)
  }

  case class WriteLine[A](line: String, then: () => ConsoleIO[A]) extends ConsoleIO[A]
  case class ReadLine[A](then: String => ConsoleIO[A]) extends ConsoleIO[A]
  case class EndWith[A](value: A) extends ConsoleIO[A]
  case class Map[A, B](v: ConsoleIO[B], f: B => A) extends ConsoleIO[A]

  val helloWorld: ConsoleIO[Int] = WriteLine("HelloWorld!", ()=>EndWith(3))

  val userNameProgram: ConsoleIO[String] = WriteLine (
    "Hello, what is your name?", () =>
    ReadLine(name =>
        WriteLine("Hello, " + name + "!", () => EndWith(name))
        )
    )

  def interpret[A](program: ConsoleIO[A]): A = program match {
    case WriteLine(line, then) => println(line); interpret(then())
    case ReadLine(then)        => interpret(then(readLine()))
    case Map(v, f)             => f(interpret(v))
    case EndWith(value)        => value
  }
}
