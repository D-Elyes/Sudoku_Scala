package Grid_Management

import Grid_Management.CellState.CellState

/**
  * This class represents a cell of a sudoku grid
 *
  * @param value : the value of the cell.
  *                the value is between 1 and 9
  *                If the value equals to 0, it means the cell is empty
  * @param suggestions : the list of suggestions the user made.
  *                      It can contain 9 values
  * @param state : Has 4 states :
  *                - INITIATED : it is when the value of the cell has been initialized by the solver (the initial filled cells in the sudoku grid)
  *                - UNVERIFIED : When the cell is emptied before the game begins, and stays in this state till a verification of the value of the cell. Gets back to this state when the value is updated
  *                - CORRECT : When the value filled by the player is correct
  *                - WRONG : when the value filled by the player is wrong
  */
case class Cell(value: Int, suggestions: List[Int] = List[Int](),state: CellState = CellState.INITIATE)

object Cell
{
  /**
    * change the state of the cell
    * @param cell : the cell that we want to change the state
    * @return ; returns a new cell with the corresponding state
    */
  def updateState(cell: Cell, newState : CellState):Cell =
  {
    cell.copy(state  = newState)
  }

  /**
    * Check if a value can be added to the suggestion list
    * @param cell : the cell in which the suggestion will be added
    * @return : true if the suggestion can be added, false if not
    */
  def checkSuggestionValueToAdd(cell: Cell):Boolean =
  {
    if(cell.suggestions.size == 9)
      return false

     true
  }

  /**
    * remove a suggestion from a cell
    * @param cell : the cell for which a suggestion will be removed
    * @param valueToRemove : the suggestion to remove
    * @return a new cell with the suggestion removed or none if the suggestions list doesn't contain the value
    */
  def removeSuggestion(cell: Cell, valueToRemove: Int): Option[Cell] =
  {
    if(cell.suggestions.contains(valueToRemove))
      {
        return Some(cell.copy(suggestions = cell.suggestions.filter(_!=valueToRemove)))
      }

    None
  }

  /**
    * replace a suggestion value by a new one
    * @param cell : the cell for which we will replace the suggestion value
    * @param oldValue : the value to replace
    * @param newValue : the new value
    * @return : a new cell with the suggestion replaced or none if the suggestions list doesn't contain the value to replace
    */
  def replace(cell: Cell, oldValue: Int, newValue: Int): Option[Cell] =
  {
    if(cell.suggestions.contains(oldValue))
      {
        val newSuggestions = cell.suggestions.map(x =>
          if(x==oldValue)
            newValue
          else
            x
        )
        return Some(cell.copy(suggestions = newSuggestions))
      }

    None
  }

  /**
    * add a new suggestion
    * @param cell : Cell that we want to add the suggestion
    * @param suggestionValue : the suggestion to add
    * @return a new cell with the suggestion added or None if the value can't be added
    */
  def addSuggestion(cell: Cell, suggestionValue: Int): Option[Cell] =
  {
    if(checkSuggestionValueToAdd(cell))
      {
        val newSuggestions = cell.suggestions :+ suggestionValue
        return Some(cell.copy(suggestions = newSuggestions))
      }

    None
  }

  /**
    * update the value of the cell
    * @param cell : the cell that we want to change the value
    * @param newValue  the new value
    * @return : a new cell with the updated value
    */
  def updateValue(cell: Cell, newValue: Int) : Cell =
  {
    cell.copy(value = newValue)
  }

  /**
    * check if a cell is empty
    * @param cell ; the cell we want to check
    * @return : true if the cell is empty (its value equals to zero) false if not
    */
  def isEmpty(cell:Cell) : Boolean=
  {
    cell.value == 0
  }

  def emptySuggestion(cell: Cell) : Cell =
  {
    cell.copy(suggestions = List[Int]())
  }


}
