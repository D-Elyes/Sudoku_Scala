package Grad_Management_Test

import Grid_Management.{Grid, Solver}
import org.scalatest.{FlatSpec, Matchers}

class SolverTest extends FlatSpec with Matchers {
  val solver = Solver()
  val grid = Grid()

  "a solver" should "contain all the values it can add in the grid (1->9" in
  {
    for(i<-1 until 9)
      {
        solver.values.contains(i) should be (true)
      }
  }

  "A solver" should "Give a valid solution" in
  {
    val solvedGrid = Solver.solveGrid(grid)
    solvedGrid match {
      case Some(solvedGrid) =>{
        solvedGrid.grid.zipWithIndex.foreach{
          case(row, indexRow) => {
            row.zipWithIndex.foreach{
              case(cell, indexColumn)=>{
                val rowCell = Grid.getRowCell(solvedGrid.grid,indexRow)
                val columnCell = Grid.getColumnCell(solvedGrid.grid,indexColumn)
                val boxCell = Grid.getBoxCell(solvedGrid.grid,indexRow,indexColumn)
                rowCell.filter(_.value ==cell.value).groupBy(identity).size should be (1)
                columnCell.filter(_.value ==cell.value).groupBy(identity).size should be (1)
                boxCell.filter(_.value ==cell.value).groupBy(identity).size should be (1)
              }
            }
          }
        }
      }
      case None =>
    }
  }

  "A solver " should "return the number of solutions" in
  {
    val solvedGrid = Solver.solveGrid(grid)
    solvedGrid match {
      case Some(grid) => {
        val testGrid = Grid.updateCellValue(grid.grid,5,5,0)
        val nbSolution = Solver.numberOfSolutions(testGrid)
          nbSolution.get should be (1)
      }
    }
  }

}
