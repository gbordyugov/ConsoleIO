object ConsoleIO {

  sealed trait ConsoleIO

  case class WriteLine(line: String, then: () => ConsoleIO) extends ConsoleIO
  case class ReadLine(then: String => ConsoleIO) extends ConsoleIO
  case class End() extends ConsoleIO

  val helloWorld: ConsoleIO = WriteLine("HelloWorld!", ()=>End())

  def interpret(program: ConsoleIO): Unit = program match {
    case WriteLine(line, then) => println(line); interpret(then())
    case ReadLine(then)        => interpret(then(readLine()))
    case End()                 => ()
  }
}
