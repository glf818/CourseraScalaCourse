import forcomp._
import Anagrams._
val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
val r = List(('l',1), ('r', 2))
subtract(lard, r)

val sentence = List("yes", "Man")
val whole = sentenceOccurrences(sentence)


def loop(sentence: Occurrences): List[List[String]] = {
  if (sentence.isEmpty) List(Nil)
  else {
    (for {oneSub <- combinations(sentence)
          oW <- dictionaryByOccurrences.getOrElse(oneSub, List())
          rM <- loop(subtract(sentence, oneSub))
          }
      yield {
         oW :: rM
      }).toSet.toList
  }
}

loop(whole)
