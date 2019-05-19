package Grid_Management

/**
  * this enumeration represent the states that a cell can have
  */
object CellState extends Enumeration {
  type CellState = Value
  val INITIATE, UNVERIFIED,CORRECT,WRONG = Value

}
