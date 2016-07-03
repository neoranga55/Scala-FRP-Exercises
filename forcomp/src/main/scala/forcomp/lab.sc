List('a', 'd', 'i', 'd', 'a', 's')
  .groupBy((c: Char) => c)
  .map{ case (c, list) => (c, list.length)}
  .toList
  .sorted


type Occurrences = List[(Char, Int)]

def subtractItem(head: (Char, Int), x: Occurrences): Occurrences =
  head match {
    case (subLetter, subCounter) =>
      for {
        (letter, counter) <- x
        if letter != subLetter || (counter - subCounter) > 0 // Filter when occurrence becomes 0
      } yield if (letter == subLetter) (letter, counter - subCounter)
      else (letter, counter)
  }

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  y match {
    case Nil => x
    case (head :: tail) => subtract(subtractItem(head, x), tail)
  }
}

val lard = List(('a', 1), ('d', 2), ('l', 1), ('r', 1))
val r = List(('d', 1), ('r', 1))

subtract(lard, r)


def letterPermutations(character: Char, times: Int): List[Occurrences] = {
  (for (taken <- 0 to times)
    yield if (taken > 0) (character, taken) :: List() else List()).toList
}

letterPermutations('a', 2)
def combineList(accumulator: Occurrences, permutations: Occurrences): List[Occurrences] = {
  permutations match {
    case Nil => List(accumulator) // Missing sorting
    case (character, times) :: tail =>
      letterPermutations(character, times).flatMap( permutationList =>
        if (permutationList.isEmpty) combineList(accumulator, tail)
        else combineList(permutationList.head :: accumulator, tail)
      )
  }
}

def combine(accumulator: Occurrences, permutations: Occurrences): List[Occurrences] = {
  permutations match {
    case Nil => List(accumulator) // Missing sorting => List(accumulator.sorted)
    case (character, times) :: tail => (for {
        taken <- 0 to times
      } yield if (taken == 0) combine(accumulator, tail)
      else combine((character, taken) :: accumulator, tail)).flatten.toList
  }
}

val c = combineList(List(), List(('a', 2), ('b', 2)))
c.mkString("\n")
c.length
val c2 = combine(List(), List(('a', 2), ('b', 2)))
c2.mkString("\n")
c2.length