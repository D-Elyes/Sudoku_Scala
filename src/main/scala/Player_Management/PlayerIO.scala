package Player_Management

import scala.annotation.tailrec
import scala.io.StdIn

object PlayerIO {

  /**
    * Convert a string to an int
    * @param s : the string to convert
    * @return : Option[Int]. If the convertion fails return None
    */
  def toInt(s : String) : Option[Int] =
  {
    try
    {
      Some(s.toInt)
    }
    catch
      {
        case e : Exception => None
      }
  }

  @tailrec
    def actionChoice() : Int=
    {
      println("1 : Choose a cell to update")
      println("2 : Solve the grid")
      println("3 : Exit")

      val x= StdIn.readLine()
      if(x == "1")
        1
      else if(x == "2" )
        2
      else if (x == "3")
        3
      else
        {
          println("Wrong choice! please choose 1,2 or 3")
          actionChoice()
        }
    }

  @tailrec
  def chooseValue(): Int = {
    val x = StdIn.readLine()
    if(toInt(x).isEmpty)
    {
      println("Input Error !!!! You have to seize a number")
      chooseValue()
    }
    else
      {
        if(toInt(x).get<1 || toInt(x).get >9)
          {
            println("Error Input !!! you must seize a number between 1 and 9")
            chooseValue()
          }
        else
          {
            toInt(x).get - 1
          }
      }
  }

  @tailrec
  def actionOnGrid(): Int =
  {
    println("1 : Add a new value to the cell")
    println("2 : See if the value of the cell is correct (you'll have to add a value first)")
    println("3 : Handle the suggestion of the cell")
    println("4 : Show the correct value fot this cell")
    println("5: Return")
    val x= StdIn.readLine()
    x match {
      case "1"=> 1
      case "2"=> 2
      case "3"=> 3
      case "4"=> 4
      case "5"=> 5
      case _ => {
        println("You have to choose 1,2,3,4 or 5")
        actionOnGrid()
      }
    }

  }

  @tailrec
  def handleSuggestion(): Int =
  {
    println("1 : Add a new suggestion")
    println("2 : Remove a suggestion")
    println("3 : Replace a suggestion")
    println("4 : Empty the list")
    println("5: Return")
    val x= StdIn.readLine()
    x match {
      case "1"=> 1
      case "2"=> 2
      case "3"=> 3
      case "4"=> 4
      case "5"=> 5
      case _ => {
        println("You have to choose 1,2,3,4 or 5")
        handleSuggestion()
      }
    }

  }

}
