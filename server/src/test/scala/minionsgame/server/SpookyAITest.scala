package minionsgame.server

import minionsgame.core._
import org.scalatest.funsuite.AnyFunSuite

class SpookyAITest extends AnyFunSuite {

  test("SpookyAI FEN conversion and communication") {
    val ai = new SpookyAI()
    ai.start()

    val game = Game.create(Game.defaultTech, Game.defaultSpells, 2, 1, false)
    val board = BoardState.create(Plane.create(10,10,HexTopology,Wall), SideArray.create(Loc(0,0), Loc(9,9)))

    val fen = SpookyAI.toFen(game, Seq(board))
    val bestMove = ai.getBestMove(fen)

    assert(bestMove.startsWith("bestmove"))

    ai.stop()
  }
}
