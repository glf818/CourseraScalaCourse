import forcomp._
import Anagrams._








val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
val r = List(('l',1), ('r', 1))

val sentence = List("i", "love", "DANCING")
val whole = sentenceOccurrences(sentence)
val z =
(for {
  oneSub <- combinations(whole)
  //oW <- dictionaryByOccurrences.getOrElse(oneSub, List())
  //rM <- dictionaryByOccurrences.getOrElse(subtract(whole, oneSub), List())
}
yield {
  (subtract(whole, oneSub), oneSub)
}).toList.sortWith((x,y)=> (x._1.length < y._1.length))

val x = whole
val y = z(1)._2
val xp = x.toMap
val yp = y.toMap
























