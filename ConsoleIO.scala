object ConsoleIO {

  sealed trait ConsoleIO[A] {
    def map[B](f: A => B): ConsoleIO[B] = Map(this, f)
    def flatMap[B](f: A => ConsoleIO[B]): ConsoleIO[B] = Chain(this, f)
  }

  case class WriteLine[A](line: String, then: () => ConsoleIO[A]) extends ConsoleIO[A]
  case class ReadLine[A](then: String => ConsoleIO[A]) extends ConsoleIO[A]
  case class Pure[A](value: A) extends ConsoleIO[A]
  case class Map[A, B](v: ConsoleIO[B], f: B => A) extends ConsoleIO[A]
  case class Chain[A, B](v: ConsoleIO[B], f: B => ConsoleIO[A]) extends ConsoleIO[A]

  val helloWorld: ConsoleIO[Int] = WriteLine("HelloWorld!", ()=>Pure(3))

  val userNameProgram: ConsoleIO[String] = WriteLine (
    "Hello, what is your name?", () =>
    ReadLine(name =>
        WriteLine("Hello, " + name + "!", () => Pure(name))
        )
    )

  def interpret[A](program: ConsoleIO[A]): A = program match {
    case WriteLine(line, then) => println(line); interpret(then())
    case ReadLine(then)        => interpret(then(readLine()))
    case Map(v, f)             => f(interpret(v))
    case Chain(v, f)           => interpret(f(interpret(v)))
    case Pure(value)           => value
  }
}

object Sequential {
  sealed trait Sequential[F[_], A] {
    def map[B](f: A => B): Sequential[F, B] = Map[F, A, B](this, f)
    def flatMap[B](f: A => Sequential[F, B]): Sequential[F, B] =
      Chain[F, A, B](this, f)
  }

  case class Effect[F[_], A](fa: F[A]) extends Sequential[F, A]
  case class Pure[F[_], A](value: A) extends Sequential[F, A]
  case class Chain[F[_], A0, A](v: Sequential[F, A0],
                                f: A0 => Sequential[F, A])
    extends Sequential[F, A]
  case class Map[F[_], A0, A](v: Sequential[F, A0], f: A0 => A)
    extends Sequential[F, A]
}
