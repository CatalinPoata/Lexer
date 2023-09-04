import Dfa.stateEquiv

import scala.collection.mutable

class Dfa[A] (var K: Set[A], var S: Set[Char], var D: Set[(A, Char, A)], var q0: A, var F: Set[A]){

  // State mapper from datatype A to datatype B
  def map[B](f: A => B) : Dfa[B] = {
    def convert(trans: (A, Char, A)): (B, Char, B) = return (f(trans._1), trans._2, f(trans._3));
    var newK = K.map(f);
    var newS = Set.empty[Char];
    var newD: Set[(B, Char, B)] = D.map(x => convert(x));
    var newq0 = f(q0);
    var newF = F.map(f);

    return new Dfa[B](newK, newS, newD, newq0, newF);
  }

  // Function that returns the next state
  def next(state:A, c: Char): A = {
    return K.filter(st => D.contains((state, c, st))).head;
  }

  // Acccepts function helper that goes through the states recursively
  def acceptsHelper(str: String, state: A): Boolean = {
    //Empty word
    if (str.length == 0) {
      return isFinal(state);
    }
    if (!S.contains(str.charAt(0))) {
      return false;
    }
    //States where the character is used
    var nextHop = next(state, str.charAt(0));
    return acceptsHelper(str.substring(1), nextHop);

    return false;
  }

  // Function that checks whether our DFA accepts a word or not
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
    return F.contains(state);
  }

  override def toString: String = {
    return "K = " + K.toString() + ", S = " + S.toString() + ", D = " + D.toString() + ", q0 = " + q0 + ", F = " + F + "\n";
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
object Dfa {
  var numberOfStates = 0;
  var stateEquiv: mutable.HashMap[Int, Set[Int]] = mutable.HashMap.empty[Int, Set[Int]];

  // Function that evaluates the text recursively
  def fromPrenex(str: String): Dfa[Int] = {
    this.numberOfStates = 0;
    this.stateEquiv = mutable.HashMap.empty;

    var newK: Set[Int] = Set.empty[Int];
    var newS: Set[Char] = Set.empty[Char];
    var newD: Set[(Int, Char, Int)] = Set.empty[(Int, Char, Int)];
    var newq0: Int = -1;
    var newF: Set[Int] = Set.empty[Int];
    var dfa = new Dfa[Int](Set.empty[Int], Set.empty[Char], Set.empty[(Int, Char, Int)], -1, Set.empty[Int]);

    //Get the nfa from prenex
    var nfa = Nfa.fromPrenex(str);
    var visited = mutable.HashMap.empty[Int, Boolean];
    for(state <- nfa.K){
      visited.put(state, false);
    }
    //Initialize a map with 2^K elements
    for (i <- 0 to (nfa.K.size * nfa.K.size - 1)) {
      stateEquiv += (i -> Set.empty[Int]);
    }

    //The first state is the epsillon closure of the initial state of the nfa, so we add it

    stateEquiv(numberOfStates) ++= nfa.epsillon(nfa.q0, visited);

    dfa.K += numberOfStates;
    dfa.q0 = numberOfStates;
    dfa.S = nfa.S;

    if (stateEquiv(numberOfStates).contains(nfa.F)) {
      dfa.F += numberOfStates;
    }
    this.numberOfStates += 1;



    for (key <- stateEquiv.keySet) {
      //Check transitions on each character
      for (c <- nfa.S) {
        var statesOnC = Set.empty[Int];
        //Get the direct states after using that character
        for (st <- stateEquiv(key)) {
          var nextStates = nfa.next(st, c);
          statesOnC ++= nfa.next(st, c);
        }

        //Get the epsillon closure of the previously found states
        var statesOnCeps = Set.empty[Int];
        for (st <- statesOnC) {
          for(key <- visited.keySet){
            visited(key) = false;
          }
          statesOnCeps ++= nfa.epsillon(st, visited);
        }

        //Append them
        statesOnC ++= statesOnCeps;
        var isFound = false;

        //If there are states that have been found, check if a state equivalent to this set of states has already been
        //found

        if (!statesOnC.isEmpty) {
          for (key2 <- stateEquiv.keySet) {
            //If such a state has been found, just add the transition
            if (stateEquiv(key2).equals(statesOnC)) {
              isFound = true;
              dfa.D += ((key, c, key2));
            }
          }
          //If the state did not exist, add it, add the transition and if it is final, add it to the set of final
          //states
          if (isFound == false) {
            stateEquiv(this.numberOfStates) ++= statesOnC;
            dfa.D += ((key, c, this.numberOfStates));
            if (statesOnC.contains(nfa.F)) {
              dfa.F += this.numberOfStates;
            }
            dfa.K += this.numberOfStates;
            numberOfStates += 1;
          }
        }
      }
    }


    //Add sink state just in case
    for (c <- dfa.S) {
      dfa.D += ((numberOfStates, c, numberOfStates));
    }
    dfa.K += numberOfStates;

    //If there are missing transitions, lead them to the sink state
    for (state <- dfa.K) {
      for (c <- dfa.S) {
        var hasTransition = false;
        for (state2 <- dfa.K) {
          if (dfa.D.contains((state, c, state2))) {
            hasTransition = true;
          }
        }
        if (!hasTransition) {
          dfa.D += ((state, c, numberOfStates));
        }
      }
    }

    return dfa;
  }
}
