package me.brandonlmorris.manscala

import io.StdIn

/** A particular state of a mancala game
  *
  * @constructor create a new mancala game.
  * @param board the values of the game board
  * @param turn the current turn's player (1 or 2)
  *
  * Created by bmorris on 10/18/16.
  */
class Manscala(val board: Board, val turn: Int) {
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

  /** Play a turn */
  def playTurn(position: Int): Manscala = {
    val (lastSide, lastPos) = board.lastPosAfterMove(turn, position)
    val newBoard = board.move(turn, position)
    val nextTurn = if (lastPos == 6) turn else if (turn == 1) 2 else 1
    val (playerSide, oppSide) =
      if (turn == 1) (newBoard.p1, newBoard.p2)
      else (newBoard.p2, newBoard.p1)
    // Capture if we can
    val finalBoard =
      if (
        lastPos != 6 &&
        lastSide == turn &&
        playerSide(lastPos) == 1 &&
        oppSide(lastPos) != 0) newBoard.capture(turn, lastPos)
      else newBoard
    new Manscala(finalBoard, nextTurn)
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
    var game = Manscala()

    // Main game loop
    while (!game.isOver) {
      println("\n" + "=" * 50)
      // Print the options on the side of the current player
      if (game.turn == 1) {
        println(game.board)
        printBoardPositions
      } else {
        printBoardPositions
        println(game.board)
      }
      print(s"Player ${game.turn}'s turn: ")
      var s = StdIn.readChar()

      // Validate the selected spot
      val playerSide = if (game.turn == 1) game.board.p1 else game.board.p2
      while (playerSide(s - 'a') == 0) {
        println("That's not a valid position. Pick a real one, bozo")
        s = StdIn.readChar()
      }
      game = game.playTurn(s - 'a')
    }

    // Print out the final score
    val (p1score, p2score) = game.finalScore
    println(s"Player 1: $p1score   Player 2: $p2score")
    val winner = if (p1score > p2score) 1 else 2
    println(s"Player $winner is the winner!")
  }

  /** Print the board options correctly aligned */
  def printBoardPositions = {
    println("       (a)   (b)   (c)   (d)   (e)   (f)")
  }

  /** Factory for a new game */
  def apply() = new Manscala(Board(), 1)
}
