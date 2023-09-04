import scala.collection.mutable
import scala.collection.mutable.HashMap

case class Lexer (spec: String) {

  // Function that returns the sink state of our DFA
  def getSinkState(dfa: Dfa[Int]): Int = {
    return dfa.K.toList.sorted.last;
  }

  // Getter for the specifications mapping
  def getSpecMapping(): mutable.LinkedHashMap[String, String] = {
    var mapping = mutable.LinkedHashMap.empty[String, String];
    var rows = spec.replaceAll("\r\n","\n").split("\n").map(row => row.stripSuffix(";"));
    for (row <- rows) {
      var parts = row.split(": ");
      if(parts(1).equals("'\\n'")){
        mapping.put(parts(0), "'\n'");
      }else {
        mapping.put(parts(0), parts(1));
      }
    }
    return  mapping;
  }

  // Getter for the DFA mapping
  def getDFAMapping(specMapping: mutable.LinkedHashMap[String, String]): mutable.LinkedHashMap[String, Dfa[Int]] = {
    var mapping = mutable.LinkedHashMap.empty[String, Dfa[Int]];
    for (key <- specMapping.keySet){
      var regex = specMapping(key);
      var dfa = Dfa.fromPrenex(Regex.toPrenex(regex));
      mapping.put(key, dfa);
    }
    return mapping;
  }

  // Getter for the NFA mapping
  def getNFAMapping(specMapping: HashMap[String, String]): HashMap[String, Nfa[Int]] = {
    var mapping = HashMap.empty[String, Nfa[Int]];
    for (key <- specMapping.keySet) {
      var regex = specMapping(key);
      var dfa = Nfa.fromPrenex(Regex.toPrenex(regex));
      println(dfa.K.size);

      mapping.put(key, dfa);
    }
    return mapping;
  }

  // Function that gets the longest matching prefix of our word
  // -1 = ok, -2 = invalid char -3 = not finished
  def getLongestMatchingPrefix(dfa: Dfa[Int], word: String, state: Int, longestMatch: String, currentMatch: String, characterNo: Int): (String, Int, Int) = {
    if(state < 0) {
      return (longestMatch, characterNo, state);
    }
    if(word.length == 0) {
      if(dfa.isFinal(state)){
        return getLongestMatchingPrefix(dfa, word, -1, currentMatch, currentMatch, characterNo);
      }
      else{
        if(longestMatch.length > 0){
          return getLongestMatchingPrefix(dfa, word, -1, longestMatch, currentMatch, characterNo);
        }else {
          return getLongestMatchingPrefix(dfa, word, -3, longestMatch, currentMatch, characterNo);
        }
      }
    }

    if(!dfa.S.contains(word.charAt(0))){
      if (dfa.isFinal(state)) {
        return getLongestMatchingPrefix(dfa, word, -2, currentMatch, currentMatch, characterNo);
      }
      else {
        return getLongestMatchingPrefix(dfa, word, -2, longestMatch, currentMatch, characterNo);
      }
    }

    var nextHop = dfa.next(state, word.charAt(0));
    if(nextHop.equals(getSinkState(dfa))){
      if (dfa.isFinal(state)) {
        return getLongestMatchingPrefix(dfa, word, -2, currentMatch, currentMatch, characterNo);
      }
      else {
        return getLongestMatchingPrefix(dfa, word, -2, longestMatch, currentMatch, characterNo);
      }
    }
    else{
      if (dfa.isFinal(nextHop)) {
        return getLongestMatchingPrefix(dfa, word.substring(1), nextHop, currentMatch :+ word.charAt(0), currentMatch :+ word.charAt(0), characterNo + 1);
      }
      else {
        return getLongestMatchingPrefix(dfa, word.substring(1), nextHop, longestMatch, currentMatch :+ word.charAt(0), characterNo + 1);
      }
    }
  }

  // Driver function that tokenizes the given text
  def lex(word: String): Either[String,List[(String,String)]] = {
    var specMapping = getSpecMapping();
    var dfaMapping = getDFAMapping(specMapping);

    var chars = 0;
    var varWord = new String(word);
    var resultList = List.empty[(String, String)];
    while(varWord.length > 0) {
      var currPrefix = "";
      var longestKey = "";
      var hasFoundWord = false;
      var hasFinishedLexing = false;
      var currchars: Int = -1;
      for(key <- dfaMapping.keySet){
        var currLong = getLongestMatchingPrefix(dfaMapping(key), varWord, dfaMapping(key).q0, "", "", chars);
        if(currLong._3 == -3){
          hasFoundWord = true;
        }
        if(currLong._3 == -1){
          hasFinishedLexing = true;
        }
        if(currLong._2 > currchars){
          currchars = currLong._2;
        }
        if(currPrefix.size < currLong._1.size){
          currPrefix = currLong._1;
          longestKey = key;
        }
      }
      if((currchars == word.length) && (chars + currPrefix.length < currchars) && (currPrefix.length == 0)){
        return Left("No viable alternative at character EOF, line " + word.count(c => c.equals('\n')));
      }

      if(currPrefix.length == 0){
        //println(resultList);
        return Left("No viable alternative at character " + currchars  +  ", line " + word.substring(0, chars).count(c => c.equals('\n')));
      }
      //println("( " + longestKey + " ) -> ( " + currPrefix + " )");

      /*if (currPrefix.length == 0) {
        println("Can not continue lexing! No suitable regex found!");
        return result;
      }*/

      chars += currPrefix.length;

      //println("( " + longestKey + " ) -> ( " + currPrefix + " )");
      resultList :+= (currPrefix, longestKey);
      varWord = varWord.substring(currPrefix.size);
    }

    return Right(resultList);
  }
}