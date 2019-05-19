import Grid_Management.{Cell, CellState, DisplayGrid, Generator, Grid, Solver}
import Player_Management.{GameState, PlayerIO}

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Random

object Main extends App {

  val newGrid = Generator.generateGrid(Grid(),Random)

  Game()
  def Game(): Unit ={
    val gameState = GameState(newGrid,newGrid)
    println("\t\tWelcome to SUDOKU\n\n")
    @tailrec
    def mainLoop(gameState: GameState) : Unit = {

      DisplayGrid.printGrid(gameState.grid)
      DisplayGrid.printSuggestion(gameState.grid)
      PlayerIO.actionChoice() match{
        case 1 =>{
          //chose a cell
          println("Row choice")
          val row = PlayerIO.chooseValue()
          println("You choose row =" +(row+1))
          println("Column choice")

          val col = PlayerIO.chooseValue()
          println("you choose column = "+(col+1))
          if(gameState.grid.grid(row)(col).state == CellState.INITIATE)
            {
              println("This cell has been initiated by the application, you can't edit initiated cells, choose")
              println("Press any key to get back to the previous menu")
              StdIn.readLine()
              mainLoop(gameState)
            }
          else
          {
            PlayerIO.actionOnGrid() match {
              case 1 =>{
                //add value to a cell
                println("Enter the value you want to add to the cell ("+(row+1)+","+(col+1)+") : ")


                val value = PlayerIO.chooseValue()
                val newGrid = Grid.updateCellValue(gameState.grid.grid,row,col,value+1)
                val newGameState = gameState.copy(grid = newGrid)
                println("Value added with success")
                println("Press any key to continue")
                StdIn.readLine()
                mainLoop(newGameState)
              }
              case 2 =>{
                //check if the value added is correct
                    if(Cell.isEmpty(gameState.grid.grid(row)(col)))
                      {
                        println("You must set a value for this cell first")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(gameState)
                      }
                    else
                      {
                        val solvedGrid = Solver.solveGrid(gameState.grid)
                        if(solvedGrid.isDefined)
                          {
                            val newGrid = Grid.updateCellState(gameState.grid.grid,row,col,CellState.CORRECT)
                            val newGameState = gameState.copy(grid = newGrid)
                            println("Correct Value !!!!!")
                            println("Press any key to continue")
                            StdIn.readLine()
                            mainLoop(newGameState)
                          }
                        else
                          {
                            val newGrid = Grid.updateCellState(gameState.grid.grid,row,col,CellState.WRONG)
                            val newGameState = gameState.copy(grid = newGrid)
                            println("Wrong Value....")
                            println("Press any key to continue")
                            StdIn.readLine()
                            mainLoop(newGameState)
                          }
                      }
              }
              case 3 =>{
                //action on the suggestions of the cell
                PlayerIO.handleSuggestion() match{
                  case 1=>{
                    //Add suggestion


                    if(Cell.checkSuggestionValueToAdd(gameState.grid.grid(row)(col)))
                      {
                        println("Choose the suggestion value")
                        val value = PlayerIO.chooseValue()
                        val newGrid = Grid.addSuggestionToCell(gameState.grid.grid,row,col,value+1)
                        val newGameState = gameState.copy(grid = newGrid.get)
                        println("Suggestion added successfully")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(newGameState)
                      }
                      else
                      {
                        println("Can't add more suggestion to this cell")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(gameState)
                      }

                  }
                  case 2=>{
                    //remove suggestion
                    if(gameState.grid.grid(row)(col).suggestions.isEmpty)
                      {
                        println("There is no value to remove")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(gameState)
                      }
                    else
                      {
                        println("Choose the suggestion value")
                        val value = PlayerIO.chooseValue()
                        val newGrid = Grid.deleteSuggestionFromCell(gameState.grid.grid,row,col,value+1)
                        val newGameState = gameState.copy(grid = newGrid.get)
                        println("Suggestion removed successfully")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(newGameState)
                      }
                  }
                  case 3=>{
                    println("Which value you want to replace ?")
                    val value = PlayerIO.chooseValue()
                    if(gameState.grid.grid(row)(col).suggestions.contains(value+1))
                      {
                        println("What is the new value ?")
                        val newValue = PlayerIO.chooseValue()
                        val newGrid = Grid.replaceSuggestionFromCell(gameState.grid.grid,row,col,value+1,newValue+1).get
                        val newGameState = gameState.copy(grid = newGrid)
                        println("Suggestion replaced successfully")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(newGameState)
                      }
                    else
                      {
                        println("This value doesn't exist")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(gameState)
                      }
                  }
                  case 4=>{
                    //empty the list of suggestion of a cell
                    if(gameState.grid.grid(row)(col).suggestions.isEmpty)
                      {
                        println("List already empty")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(gameState)
                      }
                    else
                      {
                        val newGrid = Grid.emptySuggestionOfCell(gameState.grid.grid,row,col)
                        val newGameState = gameState.copy(grid = newGrid)
                        println("List of suggestions emptied successfully")
                        println("Press any key to continue")
                        StdIn.readLine()
                        mainLoop(newGameState)

                      }
                  }
                  case 5=> mainLoop(gameState)
                }
              }
              case 4 =>{
                //get a missing value
                if(Cell.isEmpty(gameState.grid.grid(row)(col)))
                  {
                    val solvedGrid = Solver.solveGrid(gameState.initialGrid).get
                    println("The cell ("+(row+1)+","+(col+1)+") has the value : "+solvedGrid.grid(row)(col).value)
                    val newGrid = Grid.updateCellValue(gameState.grid.grid,row,col,solvedGrid.grid(row)(col).value)
                    val gridUpdate = Grid.updateCellState(newGrid.grid,row,col,CellState.INITIATE)
                    val newGameState = gameState.copy(grid = gridUpdate)
                    println("Grid updated")
                    println("Press any key to continue")
                    StdIn.readLine()
                    mainLoop(newGameState)
                  }
                else
                  {
                    println("Cell already known")
                    println("Press any key to continue")
                    StdIn.readLine()
                    mainLoop(gameState)
                  }
              }
              case 5 => mainLoop(gameState)
            }
          }

        }
        case 2 =>{
            val solvedGrid = Solver.solveGrid(gameState.initialGrid).get
            DisplayGrid.printGrid(gameState.grid)
            DisplayGrid.printCorrectGrid(gameState.grid,solvedGrid)
        }
        case 3 =>{
          println("Good Bye")
        }
      }

    }

    mainLoop(gameState)
  }
}
