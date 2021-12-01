enum TaskPart(val abbreviation: Char) {
  case First  extends TaskPart('A')
  case Second extends TaskPart('B')

  def inputFileName: String = s"input$abbreviation.txt"
}