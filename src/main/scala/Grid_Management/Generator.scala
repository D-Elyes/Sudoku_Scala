package Grid_Management

import scala.annotation.tailrec
import scala.util.Random

/**
  *This class generates a grid with a unique solution
  * @param solver
  */
case class Generator (solver: Solver)

object Generator{

  /**
    * Take an empty gird, fill it using the solver, then erase some cells
    * @param grid : the empty grid
    * @param r
    * @return : initialized grid with unique solution
    */
  def generateGrid(grid: Grid, r: Random) : Grid =
  {
    val filledGrid = Solver.solveGrid(grid)

    val gridWithErasedCells = eraseCells(filledGrid.get,r)

    gridWithErasedCells
  }

  /**
    * Erase cells till the grid has not a unique solution anymore
    * @param grid : the filled grid
    * @param r
    * @return
    */
  def eraseCells(grid: Grid, r: Random) : Grid = {
    @tailrec
    def eraseCellsTailrec(grid: Grid, r: Random, attempts : Int): Grid=
    {
      if(attempts == 0) {
        return grid
      }
      else {
        val row = r.nextInt(9)
        val col = r.nextInt(9)
        if(Cell.isEmpty(grid.grid(row)(col)))
        {
          eraseCellsTailrec(grid,r,attempts)
        }
        else {

          val newGridWithUpdateValueCell = Grid.updateCellValue(grid.grid,row,col,0)
          val newGridWithUpdateStateCell = Grid.updateCellState(newGridWithUpdateValueCell.grid,row,col,CellState.UNVERIFIED)

          if(Solver.numberOfSolutions(newGridWithUpdateStateCell).get == 1)
          {
            eraseCellsTailrec(newGridWithUpdateStateCell,r,attempts)
          }
          else
            eraseCellsTailrec(grid,r,attempts-1)
        }

      }
    }
    eraseCellsTailrec(grid,r,3)
  }

}
