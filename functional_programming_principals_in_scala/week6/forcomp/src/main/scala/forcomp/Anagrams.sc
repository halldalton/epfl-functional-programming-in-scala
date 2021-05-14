import com.sun.xml.internal.bind.v2.schemagen.xmlschema.Occurs

type Word = String

type Sentence = List[Word]

type Occurrences = List[(Char, Int)]

val dictionary: List[Word] = List("i", "love", "you", "olive", "evol")

def wordOccurrences(w: Word): Occurrences =
  (w.toLowerCase.toList groupBy (char => char)).toList map (p =>
    (p._1, p._2.length)) sortWith (_._1 < _._1)

def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences((s foldLeft "")(_ + _))

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  dictionary groupBy wordOccurrences withDefaultValue List()

def wordAnagrams(word: Word): List[Word] =
  dictionaryByOccurrences(wordOccurrences(word))

def combinations(occurrences: Occurrences): List[Occurrences] =
  if (occurrences.isEmpty) List(List())
  else {
    List() :: (for {
      split <- 1 to occurrences.length
      first <- occurrences take split
      i <- 1 to first._2
      rest <- combinations(occurrences drop split)
    } yield (first._1, i) :: rest).toSet.toList
  }

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  def subtract_pair(map: Map[Char, Int], pair: (Char, Int)):Map[Char, Int] = {
    map + (pair._1 -> (map(pair._1) - pair._2))
  }
  (y.toMap foldLeft x.toMap)(subtract_pair).toList filter (pair => pair._2 > 0) sortWith (_._1 < _._1)
}

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def getOccurrences(occurrences: Occurrences): List[List[Occurrences]] = {
    if (occurrences.isEmpty) List(List())
    else {
      val combs = combinations(occurrences) filter (comb =>
        dictionaryByOccurrences contains comb)
      (for {
        split <- 1 to combs.length
        comb <- combs take split
        rest <- getOccurrences(subtract(occurrences, comb))
      } yield comb :: rest).toSet.toList
    }
  }
  def reList(sentences: List[Sentence]): List[Sentence] = {
    if (sentences.isEmpty) List(List())
    else {
      for {
        sentence <- sentences take 1
        word <- sentence
        rest <- reList(sentences drop 1)
      } yield word :: rest
    }
  }
  if (sentence.isEmpty) List(sentence)
  else {
    val occurrences = getOccurrences(sentenceOccurrences(sentence))
    val sents = for (occurrence <- occurrences) yield occurrence map dictionaryByOccurrences
    sents flatMap reList
  }
}

val test1: Sentence = List("I", "love", "you")

sentenceAnagrams(test1)
