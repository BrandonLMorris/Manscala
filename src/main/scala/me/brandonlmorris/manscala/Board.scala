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
      if (player == 1) (p1.toArray, p2.toArray)
      else (p2.reverse.toArray, p1.reverse.toArray)
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

    // Construct the new board
    if (player == 1)
      new Board(playerPodsThirdPass.toVector, playerBankSecondPass, oppSideSecondPass.reverse.toVector, bank2)
    else
      new Board(oppSideSecondPass.toVector, bank1, playerPodsThirdPass.reverse.toVector, playerBankSecondPass)
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
