val fh = io.Source.fromURL("http://www.google.com")
val lineIterator = fh.getLines()

for(line <- lineIterator) {
  println(line)
}