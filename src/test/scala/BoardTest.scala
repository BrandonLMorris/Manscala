import org.scalatest.FlatSpec
import me.brandonlmorris.manscala.Board

/**
  * Created by bmorris on 10/19/16.
  */
class BoardTest extends FlatSpec {
  "An initial board" should "have same line lengths" in {
    val board = Board().toString
    val lengths = board.split('\n').map(_.length)
    assert(lengths.forall(_ == lengths(0)))
  }

  "Two digit numbers" should "not affect line lengths" in {
    val boardSide1 = for (i <- 1 to 6) yield 4
    val boardSide2 = for (i <- 1 to 6) yield 4
    val board = new Board(boardSide1, 0, boardSide2, 50).toString
    val lengths = board.split('\n').map(_.length)
    assert(lengths.forall(_ == lengths(0)))
  }

  "Factory creation" should "have correct values for new game" in {
    val board = Board()
    assert(board.p1.forall(_ == 4) && board.p1.length == 6)
    assert(board.p2.forall(_ == 4) && board.p2.length == 6)
    assert(board.bank1 == 0 && board.bank2 == 0)
  }

  "Board move bad player arg" should "thow exception" in {
    assertThrows[IllegalArgumentException] { Board().move(0, 0) }
    assertThrows[IllegalArgumentException] { Board().move(3, 0) }
  }

  "Board move bad pos arg" should "throw exception" in {
    assertThrows[IllegalArgumentException] { Board().move(1, 6) }
    assertThrows[IllegalArgumentException] { Board().move(1, -100) }
  }

  "Board move on empty pod" should "throw exception" in {
    assertThrows[IllegalStateException] {
      val zeros = for (i <- 1 to 6) yield 0
      new Board(zeros, 0, zeros, 0).move(1, 0)
    }
  }

  "Board move one side" should "increment pods" in {
    val board = Board().move(1, 0)
    assert(board.p2.forall(_ == 4))
    assert(board.bank1 == 0 && board.bank2 == 0)
    val expect = Vector(0, 5, 5, 5, 5, 4)
    assertSidesEqual(expect, board.p1)
  }

  "Board move one side and bank" should "increment pods and bank" in {
    val board = Board().move(1, 2)
    val expect = Vector(4, 4, 0, 5, 5, 5)
    assertSidesEqual(expect, board.p1)
    assert(board.bank1 == 1)
  }

  "Board move two sides" should "increment pods and bank" in {
    val board = Board().move(1, 3)
    val expectTop = Vector(4, 4, 4, 4, 4, 5)
    val expectBottom = Vector(4, 4, 4, 0, 5, 5)
    assertSidesEqual(board.p1, expectBottom)
    assertSidesEqual(board.p2, expectTop)
    assert(board.bank1 == 1)
  }

  "Board move player 2" should "increment pods" in {
    val board = Board().move(2, 5)
    val expect = Vector(4, 5, 5, 5, 5, 0)
    assertSidesEqual(board.p2, expect)
  }

  "Board move player 2 with bank" should "increment pods and bank" in {
    val board = Board().move(2, 3)
    assertSidesEqual(board.p2, Vector(5, 5, 5, 0, 4, 4))
    assert(board.bank2 == 1)
  }

  "Board move player 2 both sides" should "increment all pods and bank" in {
    val board = Board().move(2, 2)
    assertSidesEqual(board.p2, Vector(5, 5, 0, 4, 4, 4))
    assertSidesEqual(board.p1, Vector(5, 4, 4, 4, 4, 4))
    assert(board.bank2 == 1)
  }

  "Board move around twice" should "increment correct pods and stuff" in {
    val board = new Board(
      Vector(0, 0, 0, 0, 0, 10),
      1,
      Vector(0, 0, 0, 0, 0, 0),
      0).move(1, 5)
      assertSidesEqual(board.p2, Vector(1, 1, 1, 1, 1, 1))
      assertSidesEqual(board.p1, Vector(1, 1, 1, 0, 0, 0))
      assert(board.bank1 == 2)
      assert(board.bank2 == 0)
  }

  "Board move 3 sides" should "work fine" in {
    val board = new Board(
      (for (i <- 1 to 5) yield 0) :+ 15,
      0,
      for (i <- 1 to 6) yield 0,
      0
    ).move(1, 5)
    assertSidesEqual(board.p2, Vector(1, 1, 1, 1, 1, 2))
    assertSidesEqual(board.p1, Vector(1, 1, 1, 1, 1, 1))
    assert(board.bank1 == 2)
    assert(board.bank2 == 0)
  }

  "Board move" should "not screw this up" in {
    val side = Vector(0, 0, 0, 0, 0, 10)
    val board = new Board(side, 0, side.reverse, 0)
    val moved = board.move(1, 5)
    assertSidesEqual(moved.p2, Vector(11, 1, 1, 1, 1, 1))
    assertSidesEqual(moved.p1, Vector(1, 1, 1, 0, 0, 0))
    assert(moved.bank1 == 1)
    assert(moved.bank2 == 0)
  }

  "Calculating last position" should "work for one side of the board" in {
    val (player1, pos1) = Board().lastPosAfterMove(1, 0)
    val (player2, pos2) = Board().lastPosAfterMove(2, 5)
    assert(player1 == 1)
    assert(pos1 == 4)
  }

  "Calculating last position" should "work for movement of one" in {
    val side = Vector(1, 0, 0, 0, 0, 0)
    val (player, pos) = new Board(side, 0, side, 0).lastPosAfterMove(1, 0)
    assert(player == 1)
    assert(pos == 1)
  }

  "Calculating last position" should "work for landing in player's bank" in {
    val side = Vector(0, 0, 0, 0, 0, 1)
    assert((1, 6) == new Board(side, 0, side, 0).lastPosAfterMove(1, 5))
    assert((2, 6) == new Board(side, 0, side.reverse, 0).lastPosAfterMove(2, 0))
  }

  "Calculating last position" should "work for two sides" in {
    assert((2, 3) == Board().lastPosAfterMove(1, 5))
    assert((1, 0) == Board().lastPosAfterMove(2, 2))
  }

  "Calculating last position" should "work for three sides" in {
    val side = Vector(0, 0, 0, 0, 0, 10)
    assert((1, 2) == new Board(side, 0, side.reverse, 0).lastPosAfterMove(1, 5))
    val b = new Board(side, 0, side.reverse, 0)
    assert((2, 3) == new Board(side, 0, side.reverse, 0).lastPosAfterMove(2, 0))
  }

  "Calculating last position" should "work for three sides and bank" in {
    val side = Vector(0, 0, 0, 0, 0, 14)
    assert((1, 6) == new Board(side, 0, side, 0).lastPosAfterMove(1, 5))
  }

  "Calculating last position" should "work for three sides and bank, player 2" in {
    val side = Vector(14, 0, 0, 0, 0, 0)
    assert((2, 6) == new Board(side, 0, side, 0).lastPosAfterMove(2, 0))
  }

  "Calculating last position" should "work for fourth side, player 1" in {
    val side = Vector(0, 0, 0, 0, 0, 16)
    val b = new Board(side, 0, side.reverse, 0)
    assert((2, 4) == new Board(side, 0, side, 0).lastPosAfterMove(1, 5))
  }

  "Calculating last position" should "work for fourth side, player 2" in {
    val side = Vector(0, 0, 0, 0, 0, 16)
    val b = new Board(side, 0, side.reverse, 0)
    assert((1, 1) == new Board(side, 0, side.reverse, 0).lastPosAfterMove(2, 0))
  }

  /** Helper to check equality of two sequences */
  private def assertSidesEqual(s1: Seq[Int], s2: Seq[Int]): Unit = {
    assert(s1.length == s2.length, "Side lengths didn't match")
    assert(s1.zip(s2) forall { case (a, b) => a == b }, "Side contents don't match")
  }
}
