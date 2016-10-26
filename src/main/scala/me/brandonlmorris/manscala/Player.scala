package me.brandonlmorris.manscala

import io.StdIn
import scala.util.Random

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


/** A human player that makes moves through stdin */
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

/** A computer player that randomly selects valid moves */
class RandomComputerPlayer extends Player {
  /** Randomly pick a valid move on the board */
  def getMove(board: Board, turn: Int): Int ={
    val side = if (turn == 1) board.p1 else board.p2
    val valid_moves = side.zipWithIndex.filter{ case (value, index) => value > 0 }.map(_._2)
    val selected_move = valid_moves(Random.nextInt(valid_moves.length))
    Thread sleep 2000   // Pause for 2 seconds
    println(('a' + selected_move).toChar)
    selected_move
  }
}
