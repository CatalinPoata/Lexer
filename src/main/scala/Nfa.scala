import java.io.{File, FileWriter, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.Stack

class Nfa[A](var K: Set[A], var S: Set[Char], var D: Set[(A, Char, A)], var q0: A, var F: A) {

  // State mapper from datatype A to datatype B
  def map[B](f: A => B) : Nfa[B] = {
    def convert(trans: (A, Char, A)): (B, Char, B) = return (f(trans._1), trans._2, f(trans._3));
    var newK = K.map(f);
    var newS = Set.empty[Char];
    var newD: Set[(B, Char, B)] = D.map(x => convert(x));
    var newq0 = f(q0);
    var newF = f(F);

    return new Nfa[B](newK, newS, newD, newq0, newF);
  }

  // Function that returns the next set of possible states
  def next(state:A, c: Char): Set[A] = {
    var nextStates = Set.empty[A];
    for(transition <- D){
      if(transition._2.equals(c) && transition._1.equals(state)){
        nextStates += transition._3;
      }
    }
    return nextStates;
  }

  // Acccepts function helper that goes through the states recursively
  def acceptsHelper(str: String, state: A): Boolean = {
    // Empty word
    if (str.length == 0) {

      // No more states
      if (next(state, 'ε').isEmpty) {
        return isFinal(state);
      }
      // States remaining without using characters
      else {
        var nextHops = next(state, 'ε');
        for (hop <- nextHops) {
          var res = acceptsHelper(str, hop);
          if (res == true) {
            return true;
          }
        }
        return false;
      }
    }
    // States where the character is used
    var nextHops = next(state, str.charAt(0));
    for (hop <- nextHops) {
      var res = acceptsHelper(str.substring(1), hop);
      if (res == true) {
        return true;
      }
    }

    // States where the character is not used
    nextHops = next(state, 'ε');
    for (hop <- nextHops) {
      var res = acceptsHelper(str, hop);
      if (res == true) {
        return true;
      }
    }
    return false;
  }

  // Function that checks whether our NFA accepts a word or not
  def accepts(str: String): Boolean = {
    var currState = q0;
    return acceptsHelper(str, q0);
  }

  // Getter for the list of states
  def getStates : Set[A] = {
    return K;
  }


  // Function that checks if a state is final
  def isFinal(state: A): Boolean = {
    return state == F;
  }

  // Function that returns the epsilon closure of a state
  def epsillon(state: A, visited: mutable.HashMap[A, Boolean]): Set[A] = {
    if (next(state, 'ε').isEmpty) {
      return Set(state);
    }
    visited(state) = true;
    var partSet = Set(state);
      for (ns <- next(state, 'ε')) {
        if(visited(ns) == false) {
          partSet ++= epsillon(ns, visited);
        }
      }
    return partSet;
  }

  // toString for debugging purposes
  override def toString: String = {
    return "K = " + K.toString() + ", S = " + S.toString() + ", D = " + D.toString() + ", q0 = " + q0 + ", F = " + F + "\n";
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
object Nfa {

  // Counter class for state naming purposes
  class Counter() {
    var counter = 0;
    def count(): Int = {
      var ret = counter;
      counter += 1;
      return ret;
    }
  }

  // Function that evaluates the text recursively
  def recursiveEval(prenex: Stack[String], namer: Counter): Nfa[Int] = {

    // If size = 0 -> "" -> not defined, so I guess it returns a void NFA
    if(prenex.size != 0) {

      // Extract command type and match with types
      var command = prenex.pop();
      command match {
        case "UNION" => {
          var leftnode = recursiveEval(prenex, namer);
          var rightnode = recursiveEval(prenex, namer);
          return createUnionNfa(leftnode, rightnode, namer);
        }
        case "CONCAT" => {
          var leftnode = recursiveEval(prenex, namer);
          var rightnode = recursiveEval(prenex, namer);
          return createConcatNfa(leftnode, rightnode, namer);
        }
        case "STAR" => {
          var node = recursiveEval(prenex, namer);
          return createStarNfa(node, namer);
        }
        case "PLUS" => {
          var node = recursiveEval(prenex, namer);
          return createConcatNfa(node, createStarNfa(node, namer), namer);
        }
        case "MAYBE" => {
          var node = recursiveEval(prenex, namer);
          return createUnionNfa(node, createEpsillonNfa(namer), namer);
        }
        case "void" => {
          return createVoidNfa(namer);
        }
        case "eps" => {
          return createEpsillonNfa(namer);
        }
        case _ => {
          // This should be a character, so we check if it's in the "a" form or the "'a'" form.
          if (command.length == 1) {
            return createAtomicNfa(command.charAt(0), namer);
          }
          if(command.length == 3 && command.charAt(0) == '\'' && command.charAt(2) == '\''){
            return createAtomicNfa(command.charAt(1), namer);
          }
        }
      }
    }
    return createVoidNfa(namer);
  }

  // Function that creates a NFA that accepts a certain character
  def createAtomicNfa(c: Char, namer: Counter) : Nfa[Int] = {
    var K = Set.empty[Int];
    var S = Set.empty[Char];
    var D = Set.empty[(Int, Char, Int)];
    var q0 = -1;
    var F = -1;
    var a = namer.count();
    var b = namer.count();
    K += a;
    K += b;
    S += c;
    D += ((a, c, b));
    q0 = a;
    F = b;
    return new Nfa[Int](K, S, D, q0, F);
  }

  // Function that creates an NFA that accepts the union of 2 NFAs
  def createUnionNfa(leftNode: Nfa[Int], rightNode: Nfa[Int], namer: Counter) : Nfa[Int] = {
    var newK = Set.empty[Int];
    var newS = Set.empty[Char];
    var newD = Set.empty[(Int, Char, Int)];
    var newq0 = -1;
    var newF = -1;
    var a = namer.count();
    var b = namer.count();
    newK = leftNode.K ++ rightNode.K;
    newK += a;
    newK += b;
    newS = leftNode.S ++ rightNode.S;
    newD = leftNode.D ++ rightNode.D;
    newD += ((a, 'ε', leftNode.q0));
    newD += ((a, 'ε', rightNode.q0));
    newD += ((leftNode.F, 'ε', b));
    newD += ((rightNode.F, 'ε', b));
    newq0 = a;
    newF = b;
    return new Nfa[Int](newK, newS, newD, newq0, newF);
  }


  // Function that creates an NFA that accepts the concatenation of 2 NFAs
  def createConcatNfa(leftNode: Nfa[Int], rightNode: Nfa[Int], namer: Counter): Nfa[Int] = {
    var newK = Set.empty[Int];
    var newS = Set.empty[Char];
    var newD = Set.empty[(Int, Char, Int)];
    var newq0 = -1;
    var newF = -1;
    newK = leftNode.K ++ rightNode.K;
    newS = leftNode.S ++ rightNode.S;
    newD = leftNode.D ++ rightNode.D;
    newD += ((leftNode.F, 'ε', rightNode.q0));
    newq0 = leftNode.q0;
    newF = rightNode.F;
    return new Nfa[Int](newK, newS, newD, newq0, newF);
  }
  // Function that creates an NFA that accepts the Kleene star of an NFA
  def createStarNfa(node: Nfa[Int], namer: Counter): Nfa[Int] = {
    var newK = Set.empty[Int];
    var newS = Set.empty[Char];
    var newD = Set.empty[(Int, Char, Int)];
    var newq0 = -1;
    var newF = -1;
    var a = namer.count();
    var b = namer.count();
    newK = node.K;
    newK += a;
    newK += b;
    newS = node.S;
    newD = node.D;
    newD += ((a, 'ε', node.q0));
    newD += ((node.F, 'ε', b));
    newD += ((node.F, 'ε', node.q0));
    newD += ((a, 'ε', b));
    newq0 = a;
    newF = b;
    return new Nfa[Int](newK, newS, newD, newq0, newF);
  }

  // Function that creates an NFA that only accepts the empty string
  def createEpsillonNfa(namer: Counter): Nfa[Int] = {
    var newK = Set.empty[Int];
    var newS = Set.empty[Char];
    var newD = Set.empty[(Int, Char, Int)];
    var newq0 = -1;
    var newF = -1;
    var a = namer.count();
    newK += a;
    newq0 = a;
    newF = a;
    return new Nfa[Int](newK, newS, newD, newq0, newF);
  }

  // Function that creates an NFA that does not accept anything
  def createVoidNfa(namer: Counter): Nfa[Int] = {
    var newK = Set.empty[Int];
    var newS = Set.empty[Char];
    var newD = Set.empty[(Int, Char, Int)];
    var newq0 = -1;
    var newF = -1;
    var a = namer.count();
    var b = namer.count();
    newK += a;
    newK += b;
    newq0 = a;
    newF = b;
    return new Nfa[Int](newK, newS, newD, newq0, newF);
  }

  // Function that turns a string in the prenex form into a NFA
  def fromPrenex(str: String): Nfa[Int] =  {
    var args = str.split(" ");
    var args2: Array[String] = new Array[String](0);
    var chars = 0;
    var useLast = true;

    // Parse String
    for( i <- 0 to args.length - 2) {
      chars += args(i).length;
      if (args(i).equals("'")) {
        if (args(i+1).equals("'")) {
          if(str.charAt(chars) == ' ') {
            args2 :+= " ";
            if(i == (args.length - 2)) {
              useLast = false;
            }
          }
        }
      }else{
        args2 :+= args(i);
      }
      chars += 1;
    }

    if(useLast == true) {
      args2 :+= args.last;
    }

    //Add string to stack
    var opstack = new Stack[String];
    for (arg <- args2.reverse) {
      opstack.push(arg);
    }
    var namer = new Counter();
    return recursiveEval(opstack, namer);
  }
}