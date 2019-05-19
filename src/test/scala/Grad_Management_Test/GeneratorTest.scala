package Grad_Management_Test

import Grid_Management.{Cell, CellState, Generator, Grid, Solver}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class GeneratorTest extends FlatSpec with Matchers{

  "After erasing cells, a grid" should "have empty cells" in
  {
    val grid = Grid()
    val filledGrid = Solver.solveGrid(grid).get
    val gridWithErasedCell = Generator.eraseCells(filledGrid, Random)
    gridWithErasedCell.grid.flatten.filter(_.value == 0).nonEmpty should be (true)


  }

  "After erasing cells, a grid" should "have a unique solution" in
  {

    val grid = Grid()
    val filledGrid = Solver.solveGrid(grid).get
    val gridWithErasedCell = Generator.eraseCells(filledGrid,  Random)


    val numberSolution = Solver.numberOfSolutions(gridWithErasedCell)
    numberSolution.get should be (1)
  }

  "A new generated grid" should "have only one solution" in
  {
    val grid = Generator.generateGrid(Grid(),Random)

    Solver.numberOfSolutions(grid).get should be (1)
  }

  "A new generated grid" should "have empty cells with the state unverified and filled cells with the state initiated" in
  {
    val grid = Generator.generateGrid(Grid(),Random)

    grid.grid.flatten.foreach(cell =>
    {
      if(Cell.isEmpty(cell))
        {
          cell.value should be (0)
          cell.state should be (CellState.UNVERIFIED)
        }
      else
        {
          (cell.value != 0) should be (true)
          cell.state should be (CellState.INITIATE)
        }
    })
  }

}
