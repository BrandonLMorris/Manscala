# Manscala

A fun, classic game implemented in Scala.

Info about the mancala game can be found [here][mancala-wiki]

## To Play

1. Clone this repo (`$ git clone https://github.com/brandonlmorris/manscala`)
1. Make sure that you have [sbt][sbt-install] (Scala build tool) installed 
1. In the project main directory (`Manscala`), run `$ sbt run`

## Mancala Rules

### Setup

Mancala is played on a board with two rows of six "pods". The row closest
to the player is his/her side. Each player has their own "bank" which
is at his/her rightmost side of the board.

### Objective

The goal of mancala is to have more "seeds" in your bank than your
opponent at the end of the game. The game is over once all of either
player's pods are empty.

### Gameplay

Players alternate turns. Each turn is as follows:

- The player of the current turn selects one of his/her nonempty pods.
- The seeds in the chosen pod are removed, and distributed one-by-one
in each subsequent pod, moving counter clockwise.
- Players distribute seeds into their own bank as they progress around
the board, but do __not__ deposit seeds into their opponent's bank.
- If the last seed is deposited into a players own bank, the turn does
not alternate and the player can move again.
- If the last seed is deposited into an empty pod on the player's own
side, and the opposite pod (on the opponent's side) is nonempty, all of
the seeds in those two pods are "captured" and moved into the player's
bank. The turn then alternates to the other player.

[mancala-wiki]: http://www.scala-sbt.org/0.13/docs/Setup.html
[sbt-install]: http://www.scala-sbt.org/0.13/docs/Setup.html