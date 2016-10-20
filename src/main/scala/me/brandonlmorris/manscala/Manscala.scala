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
  /** Determine if a game is complete */
  def isOver: Boolean = {
    board.p1.forall(_ == 0) || board.p2.forall(_ == 0)
  }

  /** Caclulate the final score. Assumes the game is actually over */
  def finalScore: (Int, Int) = {
    val p1Score = board.bank1 + board.p1.sum
    val p2Score = board.bank2 + board.p2.sum
    (p1Score, p2Score)
  }
}

object Manscala {
  /** Program entry point */
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
