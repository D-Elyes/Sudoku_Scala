package Grid_Management

case class DisplayGrid()

object  DisplayGrid{

  def printCorrectGrid(grid: Grid, correctGrid: Grid):Unit = {

    printTopBorder()
    correctGrid.grid.zipWithIndex.foreach {
      case (row, rowIndex) => {
        printRowBorder()
        row.zipWithIndex.foreach{
          case(cell, colIndex) =>{
            printValueOfSolvedGird(grid,correctGrid,rowIndex,colIndex)
          }
        }
        printRowBorder()
        println()
        printBottomRowBorder(rowIndex+1,correctGrid.grid.length)
      }
    }
    printBottomBorder()
  }

  def printGrid(grid: Grid): Unit =
  {
    println("\t  1\t  2\t  3\t  4\t  5\t  6\t  7\t  8\t  9")
    print("\t")
    printTopBorder()
    grid.grid.zipWithIndex.foreach {
      case (row, rowIndex) => {
        print(rowIndex+1+"\t")
          printRowBorder()
        row.zipWithIndex.foreach{
          case(cell, colIndex) =>{
              printValue(grid,rowIndex,colIndex)
          }
        }

        printRowBorder()
        println()
        print("\t")
        printBottomRowBorder(rowIndex+1,grid.grid.length)
      }
    }
    printBottomBorder()
  }

  private def printValue(grid: Grid, row: Int, column: Int):Unit ={
    val cell = grid.grid(row)(column)
    if(Cell.isEmpty(cell))
      {
        print("   ")
        printRightColumnBorder(column+1,grid.grid.length)
      }
    else
      {
        cell.state match{
          case CellState.CORRECT =>  print(Console.GREEN+" "+cell.value+" "+Console.RESET)
          case CellState.WRONG =>  print(Console.RED++" "+cell.value+" " + Console.RESET)
          case CellState.UNVERIFIED =>  print(Console.YELLOW+" "+cell.value+" "+ Console.RESET)
          case _ => print(" "+cell.value+" ")
        }

        printRightColumnBorder(column+1,grid.grid.length)
      }
  }

  private def printValueOfSolvedGird(grid: Grid,solvedGrid: Grid, row: Int, column: Int): Unit =
  {
      val cell = grid.grid(row)(column)
      val correctCell = solvedGrid.grid(row)(column)
      if(Cell.isEmpty(cell))
     {
       print(Console.RESET+" "+correctCell.value+" ")
       printRightColumnBorder(column+1,solvedGrid.grid.length)
      }
      else
      {
        if(cell.value == correctCell.value)
          {

           if(cell.state == CellState.INITIATE)
              print(" "+correctCell.value+" ")
            else
             print(Console.GREEN+" "+correctCell.value+" "+Console.RESET)
          }
        else
          print(Console.RED++" "+correctCell.value+" " + Console.RESET)

        printRightColumnBorder(column+1,solvedGrid.grid.length)
      }
  }



  private def printTopBorder(): Unit  = {

    print("╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗\n")

  }

  private def printBottomBorder(): Unit  = {

    print("╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝\n")
  }

  private def printRowBorder(): Unit  = {
    print("║")
  }

  private def printRightColumnBorder(column : Int, size: Int): Unit =
  {

    if(column != size)
      {
        if(column % 3 ==0)
          print("║")
        else
          print("│")
      }

  }

  private def printBottomRowBorder(row: Int, size: Int) =
  {
    if(row != size)
      {
        if(row%3 ==0)
          print("╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣\n")
        else
          print("╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n")
      }

  }

   def printSuggestion(grid: Grid): Unit =
  {
    grid.grid.zipWithIndex.foreach{
      case(row,rowIndex) =>{
        row.zipWithIndex.foreach{
          case(cell,colIndex)=>{
            if(cell.suggestions.nonEmpty)
              {
                print("For the cell ("+(rowIndex+1)+","+(colIndex+1)+")you put :")
                cell.suggestions.foreach(x=> print(" "+x+" |"))
                println("\n\n")
              }
          }
        }
      }
    }
  }

}
