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
    var startSide = (if (player == 1) p1 else p2.reverse).toArray
    var otherSide = (if (player == 1) p2 else p1).toArray.reverse
    val start = if (player == 1) position else 5 - position
    if (startSide(start) == 0) throw new IllegalStateException
    var seeds = startSide(start)
    startSide(start) = 0
    var pos = start + 1

    def progress(seeds: Int, position: Int, side: Seq[Int]) = {
      val dist = math.min(seeds, 6 - position)
      val newSide = side.zipWithIndex.map {
        case (a, b) => if (b >= position && b < position + dist) a + 1 else a
      }
      (seeds - dist, newSide)
    }
    var (seeds1, startSide1) = progress(seeds, pos, startSide)
    seeds = seeds1
    startSide = startSide1.toArray

    // Stop by the bank
    var newBank = if (player == 1) bank1 else bank2
    if (seeds > 0) {
      seeds -= 1
      newBank += 1
    } else ()

    // Move around the other side of the board
    val (seeds2, otherSide2) = progress(seeds, 0, otherSide)
    seeds = seeds2
    otherSide = otherSide2.toArray

    // Skip the opponent's bank, but keep going if we can
    val (seeds3, startSide3) = progress(seeds, 0, startSide)
    seeds = seeds3
    startSide = startSide3.toArray

    // Stop by the bank
    if (seeds > 0) {
      newBank += 1
      seeds -= 1
    } else ()

    // Just to be safe
    val (seeds4, otherSide4) = progress(seeds, 0, otherSide)
    seeds = seeds4
    otherSide = otherSide4.toArray

    // Construct the new board
    if (player == 1)
      new Board(startSide.toVector, newBank, otherSide.reverse.toVector, bank2)
    else
      new Board(otherSide.toVector, bank1, startSide.reverse.toVector, newBank)
  }

  override def toString = {
    val top = p2.foldLeft("")((a, b) => a + f" $b%-3s| ").trim
    val bottom = p1.foldLeft("")((a, b) => a + f" $b%-3s| ").trim
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
