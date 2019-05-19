package Grid_Management

import scala.util.Random

/**
  * This class is a solver for a sudoku gird
  * @param values : values to use to solve a grid
  */
case class Solver(values : List[Int])

object Solver
{
  /**
    * Initialise the value list with the correct values (1 -> 9)
    * @return
    */
  def apply(): Solver =
  {
    val vals = List(1,2,3,4,5,6,7,8,9)
    val randomValues = Random.shuffle(vals)
    new Solver(randomValues)
  }

  /**
    * Solve a grid. Take a grid and return a solution of this one
    * @param grid : the grid to solve
    * @return
    */
  def solveGrid(grid: Grid) : Option[Grid] =
  {
    /**
      * iterate into the grid's cells, if a cell is empty, put the correct value for this one.
      * It will iterate till the grid if filled
      * @param grid : the grid to solve
      * @param row : the iteration row
      * @param col : the iteration column
      * @param solver : the solver
      * @return
      */
    def solveGridRec(grid: Grid,row: Int, col : Int,solver : Solver) : Option[Grid] =
    {

      if(row == 9)
        {
          Some(grid)
        }
      else
        {
          if(col==9)
            {
              solveGridRec(grid,row+1,0,solver)
            }
          else {
            if (grid.grid(row)(col).value != 0) {
              solveGridRec(grid,row,col+1,solver)
            }
            else {
              solver.values.foreach(value => {
                if (Grid.isValidForCell(grid.grid, row, col, value)) {
                  val newGrid = Grid.updateCellValue(grid.grid, row, col, value)
                  solveGridRec(newGrid, row, col + 1, solver) match {
                    case Some(returnedGrid) => return Some(returnedGrid)
                    case None =>
                  }
                }
              })
              None
            }
          }
        }
    }

    solveGridRec(grid,0,0,Solver())

  }


  /**
    * Returns the number of possible solutions
    * @param grid: the grid to check the number of solutions
    * @return
    */
  def numberOfSolutions(grid: Grid) : Option[Int] = {

    def numberOfSolutionsRec(grid: Grid,row: Int, col : Int,solver : Solver,solutions : Int) : Option[Int]=
      {
        if (row == 9) {
        return Some(solutions + 1)
         }
        else {
        if (col == 9) {
          numberOfSolutionsRec(grid, row + 1, 0, solver, solutions)
      }
        else {
          if (grid.grid(row)(col).value != 0) {
            numberOfSolutionsRec(grid, row, col + 1, solver, solutions)
             }
          else {
            solver.values.foreach(value => {
              if (Grid.isValidForCell(grid.grid, row, col, value)) {
              val newGrid = Grid.updateCellValue(grid.grid, row, col, value)
                numberOfSolutionsRec(newGrid, row, col + 1, Solver(), solutions) match {
                  case Some(solution) => {
                    val newSolver = solver.copy(values = solver.values.filter(_ != value))
                    return Some(solution + numberOfSolutionsRec(grid, row, col, newSolver, solutions).get)
                               }
                  case None =>
                   }
                }
            })
            Some(0)
          }
        }
      }
    }

    numberOfSolutionsRec(grid,0,0,Solver(),0)

  }

}
