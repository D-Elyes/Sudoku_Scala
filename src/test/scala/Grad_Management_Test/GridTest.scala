package Grad_Management_Test

import Grid_Management.{Cell, CellState, Grid}
import org.scalatest.{FlatSpec, Matchers}

class GridTest extends FlatSpec with Matchers{


  val grid = List(
    List(Cell(0),Cell(6),Cell(2),Cell(1),Cell(0),Cell(0),Cell(8),Cell(9),Cell(7)),
    List(Cell(7),Cell(0),Cell(3),Cell(0),Cell(4),Cell(6),Cell(7),Cell(8),Cell(9)),
    List(Cell(19), Cell(0),Cell(0),Cell(0),Cell(5),Cell(4),Cell(3),Cell(2),Cell(6)),
    List(Cell(18),Cell(5),Cell(6),Cell(7),Cell(8),Cell(9),Cell(1),Cell(2),Cell(3)),
    List(Cell(1),Cell(3),Cell(2),Cell(1),Cell(5),Cell(6),Cell(7),Cell(8),Cell(9)),
    List(Cell(11),Cell(4),Cell(7),Cell(8),Cell(5),Cell(2),Cell(3),Cell(6),Cell(9)),
    List(Cell(7),Cell(4),Cell(1),Cell(2),Cell(5),Cell(8),Cell(9),Cell(6),Cell(3)),
    List(Cell(8),Cell(5),Cell(2),Cell(9),Cell(6),Cell(3),Cell(7),Cell(4),Cell(1)),
    List(Cell(10),Cell(6),Cell(2),Cell(1),Cell(4),Cell(7),Cell(9),Cell(6),Cell(3)))



  "Building an empty gird" should "contains cells whose value equals to 0" in
  {
    val gridInt = List.fill(9)(List.fill(9)(0))
    val gridCell = Grid.buildEmptyGrid(gridInt)
    gridCell.nonEmpty should be (true)
    gridCell.length should be (9)
    gridCell.foreach(cells => cells.length should be (9))
    gridCell.flatten.foreach(x => x.value should be(0))
  }

  "a new grid" should "be empty" in
    {
      val grid = Grid()
      grid.grid.nonEmpty should be (true)
      grid.grid.length should be (9)
      grid.grid.foreach(cells => cells.length should be (9))
      grid.grid.flatten.foreach(x => x.value should be(0))
    }

  "the list that will have all rows values of a cell" should "contain all of the row values" in
  {

    val cellRow = Grid.getRowCell(grid,0)
    cellRow(0).value should be (0)
    cellRow(1).value should be (6)
    cellRow(2).value should be (2)
    cellRow(3).value  should be (1)
    cellRow(4).value should be (0)
    cellRow(5).value should be (0)
    cellRow(6).value  should be (8)
    cellRow(7).value  should be (9)
    cellRow(8).value  should be (7)

  }

  "the list that will have all columns values of a cell" should "contain all of the columns values" in
    {

      val cellColumn = Grid.getColumnCell(grid,0)
      cellColumn(0).value  should be (0)
      cellColumn(1).value  should be (7)
      cellColumn(2).value should be (19)
      cellColumn(3).value should be (18)
      cellColumn(4).value  should be (1)
      cellColumn(5).value should be (11)
      cellColumn(6).value should be (7)
      cellColumn(7).value should be (8)
      cellColumn(8).value should be (10)
    }

  "the list that will have all box values of a cell" should "contain all of the box values" in
    {

      val cellBox = Grid.getBoxCell(grid,0,0)
      cellBox(0).value  should be (0)
      cellBox(1).value  should be (6)
      cellBox(2).value should be (2)
      cellBox(3).value  should be (7)
      cellBox(4).value  should be (0)
      cellBox(5).value  should be (3)
      cellBox(6).value should be (19)
      cellBox(7).value should be (0)
      cellBox(8).value should be (0)
    }


  "a row containing" should "twice the same number is not valid, else, it is valid" in
  {
    Grid.isValidInRow(grid,0,4) should be (true)
    Grid.isValidInRow(grid,2,2) should be (false)
  }

  "a column containing" should "twice the same number is not valid, else, it is valid" in
    {
      Grid.isValidInColumn(grid,0,4) should be (true)
      Grid.isValidInColumn(grid,1,4) should be (false)
    }

  "a box containing" should "twice the same number is not valid, else, it is valid" in
    {
      Grid.isValidInBox(grid,1,1,4) should be (true)
      Grid.isValidInBox(grid,0,5,6) should be (false)
    }

  "a valid cell" should "should have a valid row, column and box" in
    {
      Grid.isValidForCell(grid,0,0,4) should be (true)
      Grid.isValidForCell(grid,2,1,6) should be (false)
    }

  "a grid" should "update a cell value" in
  {
    val newGird = Grid.updateCellValue(grid,5,5,99)
    newGird.grid(5)(5).value should be (99)
  }

  "a grid" should "update a cell state" in
  {
    val newGrid = Grid.updateCellState(grid,5,5,CellState.CORRECT)
    newGrid.grid(5)(5).state should be (CellState.CORRECT)
  }

  "In a grid the player" should "be able to add a suggestion for a cell" in
  {
    val newGrid = Grid.addSuggestionToCell(grid,0,0,5)
    newGrid.get.grid(0)(0).suggestions.nonEmpty should be (true)
    newGrid.get.grid(0)(0).suggestions(0) should be (5)
  }

  "In a grid, the player" should "be able to remove a suggestion from a cell" in
  {
    val newGrid = Grid.addSuggestionToCell(grid,0,0,5)
    newGrid.get.grid(0)(0).suggestions.nonEmpty should be (true)
    val anotherNewGrid = Grid.addSuggestionToCell(newGrid.get.grid,0,0,8)
    anotherNewGrid.get.grid(0)(0).suggestions(0) should be (5)
    val gridRemovedSuggestion = Grid.deleteSuggestionFromCell(anotherNewGrid.get.grid,0,0,5)
    gridRemovedSuggestion.get.grid(0)(0).suggestions(0) should be (8)
  }

  "when no needed, a cell in a grid" should "be able to empty its suggestions" in
  {
    val newGrid = Grid.addSuggestionToCell(grid,0,0,5)
    val anotherNewGrid = Grid.addSuggestionToCell(newGrid.get.grid,0,0,8)
    val emptyCellSuggestion = Grid.emptySuggestionOfCell(anotherNewGrid.get.grid,0,0)
    emptyCellSuggestion.grid(0)(0).suggestions.isEmpty should be (true)
  }


}
