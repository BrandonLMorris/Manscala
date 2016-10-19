package me.brandonlmorris.manscala

/** A particular state of a mancala game
  *
  * @constructor create a new mancala game.
  * @param board the values of the game board
  * @param turn the current turn's player (1 or 2)
  * @param gameOver true if the game is complete
  *
  * Created by bmorris on 10/18/16.
  */
class Manscala(val board: Board, val turn: Int, val gameOver: Boolean) {
  def isOver(): Boolean = {
    board.p1.forall(_ == 0) || board.p2.forall(_ == 0)
  }
}

object Manscala {
  def main(args: Array[String]) = {
    val welcomeMessage =
      """
        |Welcome to Manscala!
        |""".stripMargin
    println(welcomeMessage)
    val game = Manscala()
    println(game.board)
  }

  /** Factory for a new game */
  def apply() = new Manscala(Board(), 1, false)
}
