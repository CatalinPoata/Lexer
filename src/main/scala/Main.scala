object Main {
  def main(args: Array[String]): Unit = {

    val source = io.Source.fromFile("src/main/scala/configuration")
    val spec = try source.mkString.stripMargin('#').replaceAll("\r\n", "\n") finally source.close()

    val testsNames = (1 to 8).toList.map(num => "src/test/prog_tests/" + num + ".in")

    val words = testsNames.map(file => {
      val _source = io.Source.fromFile(file)
      try _source.mkString finally _source.close()
    })

    def transform(lexRes: Either[String, List[(String, String)]]): List[String] = {
      lexRes match {
        case Left(l) => List(l);
        case Right(l) => l.map(elem => elem._2)
      }
    }

    val results = words.map(word => transform(Lexer(spec).lex(word)))

    println(results(0));
    println(results(1));
    println(results(2));
    println(results(3));
    println(results(4));
    println(results(5));
    println(results(6));
    println(results(7));
  }
}
