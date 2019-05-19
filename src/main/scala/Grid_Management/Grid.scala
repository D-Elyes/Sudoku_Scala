package Grid_Management

import Grid_Management.CellState.CellState

import scala.annotation.tailrec

/**
  * A class representing the grid of the sudoku game
  * @param grid : thegir dof a sudoku
  */
case class Grid(grid:List[List[Cell]])

object Grid
{

  /**
    * This class will contain en empty grid, which means all of its cells will be at 0
    * @return
    */
  def apply(): Grid =
  {
    val gridInt = List.fill(9)(List.fill(9)(0))
    new Grid(buildEmptyGrid(gridInt))
  }

  /**
    * Build the empty grid by generating cell with value as 0
    * @param grid : the grid
    * @return
    */
  def buildEmptyGrid(grid : List[List[Int]]) : List[List[Cell]] =
  {
    @tailrec
    def buildEmptyGridTailRec(gridInt: List[List[Int]],gridCell: List[List[Cell]], row:Int, column: Int,rowCell: List[Cell]): List[List[Cell]] =
    {
      if(row == 9)
        return gridCell
      else
        {
          if(column == 9)
            {
              val newGridCell = gridCell :+ rowCell
               buildEmptyGridTailRec(gridInt,newGridCell,row+1,0,List[Cell]())
            }

          else
            {
              val cell = Cell(gridInt(row)(column))
              val newRowCell = rowCell :+ cell
               buildEmptyGridTailRec(gridInt,gridCell,row,column+1,newRowCell)
            }
        }
    }

    buildEmptyGridTailRec(grid,List[List[Cell]](),0,0,List[Cell]())
  }

  /**
    * get the cells of the row for the cell that the player will add a new value
    * @param grid : the grid
    * @param row : the row on which the player will add a new value
    * @return
    */
  def getRowCell(grid: List[List[Cell]], row:Int) : List[Cell] =
  {
    grid(row)
  }

  /**
    * get the cells of the column for the cell that the player will add a new value
    * @param grid : the grid
    * @param column : the column on which the player will add a new value
    * @return
    */
  def getColumnCell(grid: List[List[Cell]], column : Int) : List[Cell] =
  {
    val transposedGrid = grid.transpose
    transposedGrid(column)
  }


  /**
    * get the cells of the box (3x3 cells) for the cell that the player will add a new value
    * @param grid : the grid
    * @param row : the row on which the player will add a new value
    * @param column :  the column on which the player will add a new value
    * @return
    */
  def getBoxCell(grid: List[List[Cell]], row: Int, column : Int) : List[Cell] =
  {
    val boxGrid = grid.flatMap(_.grouped(3)).grouped(3).toList.transpose.map(_.grouped(3).toList).transpose.flatten.map(_.flatten)
    boxGrid((row/3)*3 + (column/3))
  }

  /**
    * check if the value to add can added on a row
    * @param grid : the grid
    * @param row : the row on which the player will add a new value
    * @param valueToCheck : the value to add
    * @return
    */
  def isValidInRow(grid: List[List[Cell]],row : Int, valueToCheck : Int) : Boolean =
  {
    val rowCell = getRowCell(grid,row)
    rowCell.foreach(cell => if(cell.value == valueToCheck) return false)

    true
  }

  /**
    * check if the value to add can added on a column
    * @param grid : the grid
    * @param column :  the column on which the player will add a new value
    * @param valueToCheck : the value to add
    * @return
    */
  def isValidInColumn(grid: List[List[Cell]],column : Int, valueToCheck : Int) : Boolean =
  {
    val colCell = getColumnCell(grid,column)
    colCell.foreach(cell => if(cell.value == valueToCheck) return false)

    true
  }

  /**
    * check if the value to add can added on a box
    * @param grid : the grid
    * @param row : the row of the vcell
    * @param column : the column of the cell
    * @param valueToCheck : the value to add
    * @return
    */
  def isValidInBox(grid: List[List[Cell]],row : Int, column:Int, valueToCheck : Int) : Boolean =
  {
    val boxCell = getBoxCell(grid,row,column)
    boxCell.foreach(cell => if(cell.value == valueToCheck) return false)

    true
  }

  /**
    * Check if a value can be added on a cell
    * @param grid : the grid
    * @param row : the row of the cell
    * @param column : the column of the cell
    * @param valueToCheck : the value to add
    * @return
    */
  def isValidForCell(grid: List[List[Cell]], row:Int,column:Int,valueToCheck:Int) : Boolean =
  {
    isValidInRow(grid,row,valueToCheck) && isValidInColumn(grid,column,valueToCheck) && isValidInBox(grid,row,column,valueToCheck)
  }

  /**
    * update the value of a cell
    * @param grid : the grid
    * @param row : the row of the cell
    * @param column: the column of the cell
    * @param newValue : the value to add
    * @return
    */
  def updateCellValue(grid: List[List[Cell]],row:Int,column:Int,newValue:Int): Grid =
  {
    val newCell = Cell.updateValue(grid(row)(column),newValue)
    Grid(grid.updated(row,grid(row).updated(column,newCell)))
  }

  /**
    * update the state of a cee
    * @param grid: the grid
    * @param row : the row of the cell
    * @param column: the column of the cell
    * @param newState : the new state
    * @return
    */
  def updateCellState(grid: List[List[Cell]],row: Int, column: Int, newState: CellState) : Grid =
  {
   val newCell = Cell.updateState(grid(row)(column),newState)
   Grid(grid.updated(row,grid(row).updated(column,newCell)))
  }

  /**
    * Add a suggestion in a cell
    * @param grid : the grid
    * @param row  : the row of the cell
    * @param column :  the column of the cell
    * @param valueToAdd : the suggestion to add
    * @return
    */
  def addSuggestionToCell(grid: List[List[Cell]], row: Int,column: Int, valueToAdd:Int) : Option[Grid] =
  {
    Cell.addSuggestion(grid(row)(column),valueToAdd) match
    {
      case Some(cell) => Some(Grid(grid.updated(row,grid(row).updated(column,cell))))
      case None => None
    }

  }

  /**
    * remove a suggestion from a cell
    * @param grid : the grid
    * @param row  : the row of the cell
    * @param column :  the column of the cell
    * @param valueToDelete : the value to remove
    * @return
    */
  def deleteSuggestionFromCell(grid: List[List[Cell]], row: Int, column: Int, valueToDelete: Int) : Option[Grid] =
  {
    Cell.removeSuggestion(grid(row)(column),valueToDelete) match
    {
      case Some(cell) =>  Some(Grid(grid.updated(row,grid(row).updated(column,cell))))
      case None => None
    }
  }

  /**
    * replace a suggestion value
    * @param grid : the grid
    * @param row  : the row of the cell
    * @param column :  the column of the cell
    * @param oldValue: the value to replace
    * @param newValue : the new value to add
    * @return
    */
  def replaceSuggestionFromCell(grid: List[List[Cell]], row: Int, column: Int, oldValue: Int, newValue: Int) : Option[Grid] =
  {
    Cell.replace(grid(row)(column),oldValue,newValue) match
    {
      case Some(cell) =>  Some(Grid(grid.updated(row,grid(row).updated(column,cell))))
      case None => None
    }
  }

  /**
    * Empty the suggestion list of a cell
    * @param grid : the grid
    * @param row  : the row of the cell
    * @param column :  the column of the cell
    * @return
    */
  def emptySuggestionOfCell(grid: List[List[Cell]], row: Int, column: Int) : Grid =
  {
    val newCell = Cell.emptySuggestion(grid(row)(column))
    Grid(grid.updated(row,grid(row).updated(column,newCell)))
  }



}
