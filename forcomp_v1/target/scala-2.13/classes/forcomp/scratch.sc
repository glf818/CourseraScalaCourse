
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }


type Word = String

type Sentence = List[Word]

/** `Occurrences` is a `List` of pairs of characters and positive integers saying
 *  how often the character appears.
 *  This list is sorted alphabetically w.r.t. to the character in each pair.
 *  All characters in the occurrence list are lowercase.
 *
 *  Any list of pairs of lowercase characters and their frequency which is not sorted
 *  is **not** an occurrence list.
 *
 *  Note: If the frequency of some character is zero, then that character should not be
 *  in the list.
 */
type Occurrences = List[(Char, Int)]

/** The dictionary is simply a sequence of words.
 *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
 */
val dictionary: List[Word] = loadDictionary

/** Converts the word into its character occurrence list.
 *
 *  Note: the uppercase and lowercase version of the character are treated as the
 *  same character, and are represented as a lowercase character in the occurrence list.
 *
 *  Note: you must use `groupBy` to implement this method!
 */