import org.scalatest.FlatSpec
import me.brandonlmorris.manscala.{Board, Manscala}

/**
  * Created by bmorris on 10/19/16.
  */
class ManscalaTest extends FlatSpec {
  "An initial game" should "start with player one" in {
    val game = Manscala()
    assert(game.turn == 1)
    assert(!game.gameOver)
  }

  "A game with no seeds on one side" should "be over" in {
    val zeros = for (i <- 1 to 6) yield 0
    val nonzeros = for (i <- 1 to 6) yield 1
    assert(new Manscala(new Board(zeros, 0, nonzeros, 0), 1, false).isOver)
    assert(new Manscala(new Board(nonzeros, 0, zeros, 0), 1, false).isOver)
    assert(!new Manscala(new Board(nonzeros, 0, nonzeros, 0), 1, false).isOver)
  }

  "The final score" should "add all the pod sides and banks" in {
    val (s1, s2) = Manscala().finalScore
    assert(s1 == 24)
    assert(s2 == 24)
  }

  "The final score" should "add all the pod sides and banks (2)" in {
    val (s1, s2) = new Manscala(
      new Board(Vector(0, 0, 0, 0, 0, 0), 10, Vector(9, 9, 9, 9, 9, 9), 9),
      1,
      false).finalScore
    assert(s1 == 10)
    assert(s2 == 63)
  }
}
