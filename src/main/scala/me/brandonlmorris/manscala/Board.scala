package me.brandonlmorris.manscala

/**
  * The board of an ongoing game of mancala
  *
  * @param p1: The number of seeds in each of lower player's pods (left to right)
  * @param bank1: The number of seeds in the lower player's bank
  * @param p2: The number of seeds in each of upper player's pods (left to right, from that player's perspective)
  * @param bank2: The number of seeds in the upper player's bank
  *
  * Created by bmorris on 10/18/16.
  */
class Board(val p1: Seq[Int], val bank1: Int, val p2: Seq[Int], val bank2: Int) {

  /** Create a new board from a player taking a turn */
  def move(player: Int, position: Int): Board = {
    if (player != 1 && player != 2) throw new IllegalArgumentException
    if (position < 0 || position > 5) throw new IllegalArgumentException

    // Redistribute the pieces. Should look for more functional way
    // Board movement positions are always counted from the left
    val startSide = (if (player == 1) p1 else p2.reverse).toArray
    val otherSide = (if (player == 1) p2 else p1).toArray.reverse
    val start = if (player == 1) position else (5 - position)
    if (startSide(start) == 0) throw new IllegalStateException
    var seeds = startSide(start)
    startSide(start) = 0
    var pos = start + 1
    // Fist go around
    while (seeds > 0 && pos < 6) {
      startSide(pos) += 1
      pos += 1
      seeds -= 1
    }

    // Stop by the bank
    var firstBank = if (player == 1) bank1 else bank2
    var secondBank = if (player == 1) bank2 else bank1
    if (seeds > 0) {
      seeds -= 1
      firstBank += 1
    } else ()

    // Move around the other side of the board
    pos = 0
    while (seeds > 0 && pos < 6) {
      otherSide(pos) += 1
      pos += 1
      seeds -= 1
    }

    // Skip the opponent's bank, but keep going if we can
    pos = 0
    while (seeds > 0 && pos < 6) {
      startSide(pos) += 1
      pos += 1
      seeds -= 1
    }

    // Stop by the bank
    if (seeds > 0) {
      firstBank += 1
      seeds -= 1
    }

    // Just to be safe
    pos = 0
    while (seeds > 0 && pos < 6) {
      otherSide(pos) += 1
      pos += 1
      seeds -= 1
    }

    // Construct the new board
    val newP1 = (if (player == 1) startSide else otherSide).toVector
    val newP2 = (if (player == 2) startSide.reverse else otherSide.reverse).toVector
    val b1 = if (player == 1) firstBank else secondBank
    val b2 = if (player == 1) secondBank else firstBank
    new Board(newP1, b1, newP2, b2)
  }

  override def toString = {
    val top = p2.foldLeft("")((a, b) => a + f" $b%-3s| ").trim
    val bottom = p1.foldLeft("")((a, b) => a + f" $b%-3s| ").trim
    f"""
    >${"-" * 47}
    >|    |  $top    |
    >| $bank2%-3s|                 |                 | $bank1%-3s|
    >|    |  $bottom    |
    >${"-" * 47}
    >""".stripMargin('>').trim
  }
}

/**
  * Board companion object. Used as a factory to create a starting board easily
  */
object Board {
  /** Create an initial board */
  def apply() = {
    val boardSide = for (i <- 1 to 6) yield 4
    new Board(boardSide, 0, boardSide, 0)
  }
}
