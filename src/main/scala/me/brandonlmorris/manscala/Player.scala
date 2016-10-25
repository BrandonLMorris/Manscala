package me.brandonlmorris.manscala

import io.StdIn

/** A player of the mancala game
  *
  * Created by bmorris on 10/24/16.
  */
trait Player {
  /** Get a player's selected move for a game
    *
    * @param board the current game configuration
    * @param turn the current player's turn number (0 or 1)
    * @return a selection, an integer from 0 to 5
    */
  def getMove(board: Board, turn: Int): Int
}


class HumanPlayer extends Player {
  /** Read a human's move selection from stdin */
  def getMove(board: Board, turn: Int): Int = {
    var s: Int = StdIn.readChar - 'a'
    while (s < 0 || s > 5) {
      println("That's not a valid position. Pick a real one, bozo")
      s = StdIn.readChar - 'a'
    }
    s
  }
}
