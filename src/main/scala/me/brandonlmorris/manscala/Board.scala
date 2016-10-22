package me.brandonlmorris.manscala

/**
  * The board of an ongoing game of mancala
  *
  * @param p1: The number of seeds in each of lower player's pods (left to right)
  * @param bank1: The number of seeds in the lower player's bank
  * @param p2: The number of seeds in each of upper player's pods (left to right, from that player's perspective)
  * @param bank2: The number of seeds in the upper player's bank
  * @return a new board after the move takes place
  *
  * Created by bmorris on 10/18/16.
  */
class Board(val p1: Seq[Int], val bank1: Int, val p2: Seq[Int], val bank2: Int) {

  /** Create a new board from a player taking a turn */
  def move(player: Int, position: Int): Board = {
    if (player != 1 && player != 2) throw new IllegalArgumentException
    if (position < 0 || position > 5) throw new IllegalArgumentException

    // Inner function for depositing seeds along a side
    def progress(seeds: Int, position: Int, side: Seq[Int]) = {
      val dist = math.min(seeds, 6 - position)
      val newSide = side.zipWithIndex.map {
        case (a, b) => if (b >= position && b < position + dist) a + 1 else a
      }
      (seeds - dist, newSide)
    }
    // Inner function for depositing a seed in the bank
    def depositInBank(seeds: Int, bank: Int): (Int, Int) = {
      if (seeds > 0) (seeds - 1, bank + 1) else (seeds, bank)
    }

    // Redistribute the pieces. Board movement positions are always counted
    // from the left
    val (startSide, oppSide) =
      if (player == 1) (p1.toArray, p2.reverse.toArray)
      else (p2.reverse.toArray, p1.toArray)
    val start = if (player == 1) position else 5 - position
    val playerBank = if (player == 1) bank1 else bank2
    if (startSide(start) == 0) throw new IllegalStateException
    val seeds = startSide(start)
    startSide(start) = 0
    val pos = start + 1

    // First side pass
    val (seedsFirstPass, playerPodsFirstPass) = progress(seeds, pos, startSide)
    val (seedsFirstPassAndBank, playerBankFirstPass) = depositInBank(seedsFirstPass, playerBank)

    // Move around the other side of the board
    val (seedsSecondPass, oppSideSecondPass) = progress(seedsFirstPassAndBank, 0, oppSide)

    // Skip the opponent's bank, but keep going if we can
    val (seedsThirdPass, playerPodsThirdPass) = progress(seedsSecondPass, 0, playerPodsFirstPass)

    // Stop by the bank
    val (seedsThirdsPassAndBank, playerBankSecondPass) = depositInBank(seedsThirdPass, playerBankFirstPass)

    // Just to be safe
    val (seedsFourthPass, oppSideFourthPass) = progress(seedsThirdsPassAndBank, 0, oppSideSecondPass)

    // Make our assumption explicit
    assert(seedsFourthPass == 0, "Still seeds after fourth pass, did not account for that")

    // Construct the new board
    if (player == 1)
      new Board(playerPodsThirdPass.toVector, playerBankSecondPass, oppSideFourthPass.reverse.toVector, bank2)
    else
      new Board(oppSideFourthPass.toVector, bank1, playerPodsThirdPass.reverse.toVector, playerBankSecondPass)
  }

  /** Capture pieces from a mancala game
    *
    * Pieces from the capturing pod and the opposite pod (same position, other player) are all emptied and moved
    * to the capturing player's bank.
    *
    * @param player the side (1 or 2) doing the capturing
    * @param pos the position doing the capturing
    * @return a new mancala board after the capture
    */
  def capture(player: Int, pos: Int): Board = {
    assert(p1(pos) != 0 && p2(pos) != 0)
    val (newBank1, newBank2) =
      if (player == 1)
        (bank1 + p1(pos) + p2(pos), bank2)
      else
        (bank1, bank2 + p1(pos) + p2(pos))
    new Board(p1.updated(pos, 0), newBank1, p2.updated(pos, 0), newBank2)
  }

  /** Calculate the last pod that will be visited by a move
    *
    * @param player the number (1, 2) of the player making the move
    * @param pos the index of the move about to be made (always counting from the left)
    * @return a tuple (player, pos) that represents the players side and position the last seed will be deposited in.
    *         A position of 6 represents a player's bank.
    */
  def lastPosAfterMove(player: Int, pos: Int): (Int, Int) = {
    val seeds = (if (player == 1) p1 else p2)(pos)
    val distToBank = if (player == 1) 6 - pos else pos + 1

    // Could use a better approach
    if (player == 1)
      if (seeds < distToBank) (1, pos + seeds)
      else if (seeds == distToBank) (1, 6)
      else if (seeds <= distToBank + 6) (2, 6 - (seeds - distToBank))
      else if (seeds <= distToBank + 12) (1, seeds - distToBank - 7)
      else if (seeds == distToBank + 13) (1, 6)
      else if (seeds <= distToBank + 19) (2, 19 - (seeds - distToBank))
      else { assert(false); (0, 0) }
    else
      if (seeds < distToBank) (2, pos - seeds)
      else if (seeds == distToBank) (2, 6)
      else if (seeds <= distToBank + 6) (1, seeds - distToBank - 1)
      else if (seeds <= distToBank + 12) (2, 12 - (seeds - distToBank))
      else if (seeds == distToBank + 13) (2, 6)
      else if (seeds <= distToBank + 19) (1, seeds - distToBank - 14)
      else { assert(false); (0, 0) }
  }

  override def toString = {
    val top = p2.foldLeft("")((a, b) => a + StringContext(" ", "%-3s| ").f(b)).trim
    val bottom = p1.foldLeft("")((a, b) => a + StringContext(" ", "%-3s| ").f(b)).trim
    f"""
    >${"-" * 47}
    >|    |  $top    |
    >| $bank2%-3s|${" " * 17}|${" " * 17}| $bank1%-3s|
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
