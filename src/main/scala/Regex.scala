import scala.collection.mutable
import scala.collection.mutable.Stack

object Regex {

  // Function that preprocesses a regex
  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    var prepList = List.empty[Either[Char, Char]];
    if(s.length == 3 && s(0) == 'e' && s(1) == 'p' && s(2) == 's'){
      return prepList :+ Left('ε');
    }
    var iter = 0;
    while(iter < s.length){
      var character = s(iter);
      character match{
        case '(' | ')' | '*' | '|' | ' ' | '+' | '?' | '[' | ']' => {
          if(iter > 0 && iter < s.length - 1) {
            if(s(iter - 1).equals('\'') && s(iter + 1).equals('\'')){
              prepList :+= Left(character);
            }else{
              if (!character.equals(' ')) {
                prepList :+= Right(character);
              }
            }
          }else{
            if(!character.equals(' ')) {
              prepList :+= Right(character);
            }
          }
        }
        case '\\' => {
          if(iter > 1 && s.length > 3  && (iter <= s.length - 3) && s(iter - 1).equals('\'') && s(iter + 1).equals('n') && s(iter + 2).equals('\'')){
            prepList :+= Left('\n');
            iter += 2;
          }
          else{
            if (iter > 1 && s.length > 3 && (iter <= s.length - 3) && s(iter - 1).equals('\'') && s(iter + 1).equals('t') && s(iter + 2).equals('\'')) {
              prepList :+= Left('\t');
              iter += 2;
            }
            else {
              prepList :+= Left('\\');
            }
          }
        }
        case '\'' => {
          var okEscape = false;
          var okNewline = false;
          //Is first or second
          if(((iter == 0) && (s.length > 2)) || ((iter == 1) && (s.length > 3))) {
            if(!s(iter + 2).equals('\'')){
              okEscape = true;
            }
          }

          if (((iter == 0) && (s.length > 3)) || ((iter == 1) && (s.length > 4))) {
            if (!s(iter + 3).equals('\'') && !s(iter + 1).equals('\\')) {
              okNewline = true;
            }
          }


          //Is last or 2nd to last character
          if (((s.length > 2) && (iter == (s.length - 1))) || ((s.length > 3) && (iter == (s.length - 2)))) {
            if (!s(iter - 2).equals('\'')) {
              okEscape = true;
            }
          }

          if (((s.length > 3) && (iter == (s.length - 1))) || ((s.length > 4) && (iter == (s.length - 2)))) {
            if (!s(iter - 3).equals('\'') && !s(iter - 2).equals('\\')) {
              okNewline = true;
            }
          }

          //Appears after the 2nd character and before the 2nd to last character
          if(((s.length > 1 && iter > 1 && iter < s.length - 2) && (!s(iter - 2).equals('\'') && !s(iter + 2).equals('\'')))) {
            if(!s(iter - 2).equals('\'') && !s(iter + 2).equals('\'')){
              okEscape = true;
            }
          }

          if((s.length > 3 && iter < s.length - 4)){
            if(!s(iter + 3).equals('\'')){
              okNewline = true;
            }
          }

          if((okEscape == true && okNewline == true)){
            prepList :+= Left('\'');
          }
        }
        case _ => {
          if (character == 'e' && iter < s.length - 2 && s(iter + 1) == 'p' && s(iter + 2) == 's') {
            prepList :+= Left('ε');
            iter += 2;
          }
          else {
            prepList :+= Left(character);
          }

        }
      }
      iter += 1;
    }
    var interList = changeSyntacticSugars(prepList);
    var interList2 = changeNewOps(interList);
    return addConcats(interList2);
  }

  // Function that processes syntactic sugars
  def changeSyntacticSugars(s: List[Either[Char, Char]]): List[Either[Char, Char]] = {
    var iter = 0;
    var fs = List.empty[Either[Char, Char]];
    while (iter < s.length) {
      var character = s(iter);
      character match {
        case Right(']') => {
          fs = fs.dropRight(4);
          var partList = List.empty[Either[Char, Char]];
          for (i <- s(iter - 3).merge to s(iter - 1).merge) {
            partList :+= Right('|');
            partList :+= Left(i);
          }
          partList = partList.drop(1);
          partList +:= Right('(');
          partList :+= Right(')');
          fs :++= partList;
        }

        case _ => {
          fs :+= character;
        }
      }
      iter += 1;
    }
    return fs;
  }

  // Function that processes the + and ? operators
  def changeNewOps(s: List[Either[Char, Char]]): List[Either[Char, Char]] = {
    var iter = 0;
    var fs = List.empty[Either[Char, Char]];
    while(iter < s.length) {
      var character = s(iter);
      character match {
        case Right('+') => {
          if(iter > 0) {
            if(!fs(fs.length - 1).equals(Right(')'))){
              fs = fs.dropRight(1);
              fs :++= List(Right('('), s(iter - 1), s(iter - 1), Right('*'), Right(')'));
            }else{
              var stack = Stack[Either[Char, Char]]();
              stack.push(fs(fs.length - 1));
              var prevExpr = List(fs(fs.length - 1), Right('*'));
              var revIter = fs.length - 2;
              while(!stack.isEmpty){
                if(fs(revIter).equals(Right(')'))){
                  stack.push(fs(revIter));
                }
                if(fs(revIter).equals(Right('('))){
                  stack.pop();
                }
                prevExpr +:= fs(revIter);
                revIter -= 1;
              }
              fs = fs.dropRight(prevExpr.size - 1);
              fs :+= Right('(');
              fs :++= prevExpr;
              fs = fs.dropRight(1);
              fs :++= prevExpr;
              fs :+= Right(')');
            }
          }
        }

        case Right('?') => {
          if (iter > 0) {
            if (!fs(fs.length - 1).equals(Right(')'))) {
              fs = fs.dropRight(1);
              fs :++= List(Right('('), s(iter - 1), Right('|'), Left('ε'), Right(')'));
            } else {
              var stack = Stack[Either[Char, Char]]();
              stack.push(fs(fs.length - 1));
              var prevExpr = List(fs(fs.length - 1));
              var revIter = fs.length - 2;
              while (!stack.isEmpty) {
                if (fs(revIter).equals(Right(')'))) {
                  stack.push(fs(revIter));
                }
                if (fs(revIter).equals(Right('('))) {
                  stack.pop();
                }
                prevExpr +:= fs(revIter);
                revIter -= 1;
              }
              fs = fs.dropRight(prevExpr.size);
              fs :+= Right('(');
              fs :++= prevExpr;
              fs :+= Right('|');
              fs :+= Left('ε');
              fs :+= Right(')');
            }
          }
        }

        case _ => {
          fs :+= character;
        }
      }
      iter += 1;
    }
    return fs;
  }

  // Function that concatenates the groups of operands and adjacent characters
  def addConcats(s: List[Either[Char, Char]]): List[Either[Char, Char]] = {
    var fs = List.empty[Either[Char, Char]];
    if(s.length == 0){
      return fs;
    }
    var iter = 0;
    while (iter < s.length - 1) {
      var elem = s(iter);
      var nextElem = s(iter + 1);
      fs :+= elem;
      elem match {
        case Left(_) | Right('*') => {
          nextElem match {
            case Left(_) | Right('(')  => {
              fs :+= Right('-');
            }
            case _ => {

            }
          }
        }
        case Right(')') => {
          nextElem match {
            case Right('(') | Left(_) => {
              fs :+= Right('-');
            }
            case _ => {

            }
          }
        }
        case _ => {

        }
      }
      iter += 1;
    }
    fs :+= s(s.length - 1);
    return fs;
  }

  // Function that turns the text from the infix form to the prefix form
  def infixToPrefix(s: List[Either[Char, Char]]): String = {
    var prefix = "";
    var revInfix = s.reverse;
    var opStack = Stack.empty[Char];
    var precedence = mutable.HashMap('(' -> 3, ')' -> 3, '*' -> 2, '-' -> 1, '|' -> 0);
    for (elem <- revInfix) {
      elem match {
        case Right(')') => {
          opStack.push(elem.merge);
        }
        case Right('(') => {
          while (opStack.top != ')') {
            opStack.pop() match {
              case '*' => prefix += "RATS ";
              case '-' => prefix += "TACNOC ";
              case '|' => prefix += "NOINU ";
              case _ => {}
            }
          }
          if(!opStack.isEmpty){
            opStack.pop();
          }
        }
        case Right('*') | Right('-') | Right('|') => {
          if (opStack.isEmpty || opStack.top == ')' || precedence(elem.merge) >= precedence(opStack.top)) {
            opStack.push(elem.merge);
          }else{
            if(precedence(elem.merge) < precedence(opStack.top)){
              while(!opStack.isEmpty && opStack.top != ')' && opStack.top != '(' && precedence(elem.merge) < precedence(opStack.top)){
                opStack.pop() match {
                  case '*' => prefix += "RATS ";
                  case '-' => prefix += "TACNOC ";
                  case '|' => prefix += "NOINU ";
                  case _ => {}
                }
              }
            }
            opStack.push(elem.merge);
          }
        }

        case Left('ε') => {
          prefix += "spe ";
        }

        case Left(' ') => {
          prefix += "' ' ";
        }

        case Left('\'') => {
          prefix += "'\'' ";
        }

        case Left('\n') => {
          prefix += "'\n' ";
        }

        case Left(value) => {
          prefix += value + " ";
        }

        case _ => {

        }
      }
    }

    while (!opStack.isEmpty) {
      opStack.pop() match {
        case '*' => prefix += "RATS ";
        case '-' => prefix += "TACNOC ";
        case '|' => prefix += "NOINU ";
        case _ => {}
      }
    }

    return prefix.dropRight(1).reverse;
  }


  // This function constructs a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    var infix = preprocess(str.toList);
    infixToPrefix(infix);
  }
}
