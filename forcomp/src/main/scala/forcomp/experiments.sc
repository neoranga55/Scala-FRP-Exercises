object experiments {
  List('a', 'd', 'i', 'd', 'a', 's')
    .groupBy((c: Char) => c)
    .map{ case (c, list) => (c, list.length)}
    .toList
    .sorted

}