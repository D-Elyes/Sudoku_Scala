package Grad_Management_Test

import Grid_Management.{Cell, CellState}
import org.scalatest.{FlatSpec, Matchers}

class CellTest extends FlatSpec with Matchers {

  val cell : Cell = Cell(0)

  "a new cell" should "be empty" in
  {
    Cell.isEmpty(cell) should be (true)
  }

  "when cell value different from 0" should "not be empty" in
  {
    val cellUpdateValue = Cell.updateValue(cell,1)
    Cell.isEmpty(cellUpdateValue) should be (false)

  }

  "a new cell " should "have empty suggestions" in
  {
    cell.suggestions.length should be (0)
  }

  "After adding a suggestion, the list length" should "be superior to 0" in
  {
    val cellUpdateSuggestion = Cell.addSuggestion(cell,5)
    cellUpdateSuggestion.get.suggestions.nonEmpty should be (true)
    cellUpdateSuggestion.get.suggestions.contains(5) should be (true)

  }

  "After replacing a value in the suggestion, the list" should "contain the new value" in
  {
    val cellAddSuggestion = Cell.addSuggestion(cell,5)
    val cellReplaceSuggestion = Cell.replace(cellAddSuggestion.get,5,8)
    cellReplaceSuggestion.get.suggestions.contains(8) should be (true)
    cellReplaceSuggestion.get.suggestions.contains(5) should be (false)
  }

  "A suggestion list" should "not replace a value it doesn't contain" in
  {
    Cell.replace(cell,5,3) should be (None)
  }

  "After removing a value from suggestion, the list" should "not contain this value anymore" in
  {
    val cellAddSuggestion = Cell.addSuggestion(cell,5)
    val cellAddAnotherSuggestion = Cell.addSuggestion(cellAddSuggestion.get,8)
    val cellRemoveSuggestion = Cell.removeSuggestion(cellAddAnotherSuggestion.get,5)
    cellRemoveSuggestion.get.suggestions.length should be (1)
    cellRemoveSuggestion.get.suggestions.contains(5) should be (false)
    cellRemoveSuggestion.get.suggestions.contains(8) should be (true)
  }

  "Suggestion list" should "not remove values that it doesn't contain" in
  {
    val tryRemove = Cell.removeSuggestion(cell,5)
    tryRemove should be (None)
  }

  "A suggestion list" should "contains only 9 values" in
  {
    val cellWithFullSuggestion = cell.copy(suggestions = List.fill(9)(5))
    Cell.checkSuggestionValueToAdd(cellWithFullSuggestion) should be (false)
    Cell.addSuggestion(cellWithFullSuggestion,5) should be (None)
  }

  "A new cell" should "initially be in INITIATED state  " in
  {
    cell.state should be (CellState.INITIATE)
  }

  "A cell " should "change its initial value when assigned a new one" in
  {
    val cellInitial = Cell.updateState(cell,CellState.UNVERIFIED)
    cellInitial.state should be (CellState.UNVERIFIED)
  }

  "After emptying the suggestions list,a cell" should "have its suggesions list size equel to 0" in
  {
    val cellWithFullSuggestion = cell.copy(suggestions = List.fill(9)(5))
    val cellWithEMptySuggestion = Cell.emptySuggestion(cellWithFullSuggestion)
    cellWithEMptySuggestion.suggestions.length should be (0)
  }



}
