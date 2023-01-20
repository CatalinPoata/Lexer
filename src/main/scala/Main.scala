object Main {
  def main(args: Array[String]): Unit = {
    /*val spec =
      """BEGIN: (((' ')|('\n'))*)begin(((' ')|('\n'))*);
        ##END: (((' ')|('\n'))*)end(((' ')|('\n'))*);
        ##EQUAL: (((' ')|('\n'))*)==(((' ')|('\n'))*);
        ##ASSIGN: (((' ')|('\n'))*)=(((' ')|('\n'))*);
        ##PLUS: (((' ')|('\n'))*)('+')(((' ')|('\n'))*);
        ##MINUS: (((' ')|('\n'))*)('-')(((' ')|('\n'))*);
        ##MULTIPLY: (((' ')|('\n'))*)('*')(((' ')|('\n'))*);
        ##GREATER: (((' ')|('\n'))*)>=(((' ')|('\n'))*);
        ##WHILE: (((' ')|('\n'))*)while(((' ')|('\n'))*);
        ##DO: (((' ')|('\n'))*)do(((' ')|('\n'))*);
        ##OD: (((' ')|('\n'))*)od(((' ')|('\n'))*);
        ##IF: (((' ')|('\n'))*)if(((' ')|('\n'))*);
        ##THEN: (((' ')|('\n'))*)then(((' ')|('\n'))*);
        ##ELSE: (((' ')|('\n'))*)else(((' ')|('\n'))*);
        ##FI: (((' ')|('\n'))*)fi(((' ')|('\n'))*);
        ##RETURN: (((' ')|('\n'))*)return(((' ')|('\n'))*);
        ##OPEN_PARANTHESIS: (((' ')|('\n'))*)('(')(((' ')|('\n'))*);
        ##CLOSE_PARANTHESIS: (((' ')|('\n'))*)(')')((' ')|('\n'))* ;
        ##NUMBER: NUMBER: (((' ')|('\n'))*)([0-9])+(((' ')|('\n'))*);
        ##VARIABLE: VARIABLE: (((' ')|('\n'))*)([a-z]|[A-Z])(((' ')|('\n'))*);
        #""".stripMargin('#')*/

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
    //assert(Lexer(spec).lex("begin\na = 1\nend") == List("BEGIN", "VARIABLE", "ASSIGN", "NUMBER", "END"))
    /*assert(Lexer(spec).lex("a") == Right(List(("a", "A"))))
    assert(Lexer(spec).lex("aa") == Right(List(("a", "A"), ("a", "A"))))
    assert(Lexer(spec).lex("abca") == Right(List(("a", "A"), ("bc", "BC"), ("a", "A"))))
    assert(Lexer(spec).lex("abcdefdefbca") == Right(List(("a", "A"), ("bc", "BC"), ("def", "DEF"), ("def", "DEF"), ("bc", "BC"), ("a", "A"))))*/

  }
}
