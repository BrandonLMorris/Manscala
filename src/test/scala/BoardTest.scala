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

  def assertSidesEqual(s1: Seq[Int], s2: Seq[Int]): Unit = {
    assert(s1.length == s2.length, "Side lengths didn't match")
    assert(s1.zip(s2) forall { case (a, b) => a == b }, "Side contents don't match")
  }
}
