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
}
