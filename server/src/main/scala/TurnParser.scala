package minionsgame.server

import minionsgame.core._
import minionsgame.core.PerformTech

class TurnParser(game: GameState, makeActionId: () => String) {
  private val movers: Array[scala.collection.mutable.Map[Loc, Piece]] =
    Array.fill(game.numBoards)(scala.collection.mutable.Map())

  def convertUMIMoveToActions(
      umiMove: String
  ): List[Protocol.Query] = {
    val parts = umiMove.split(" ")

    if (parts(0) == "endturn") {
      return (0 until game.numBoards)
        .map(i => Protocol.DoGameAction(SetBoardDone(i, true)))
        .toList
    }

    if (parts(0) != "action") {
      return List()
    }

    val actionType = parts(1)
    val actionId = makeActionId()

    actionType match {
      case "board" =>
        val boardIdx = parts(2).toInt
        val boardActionType = parts(3)

        boardActionType match {
          case "setup" =>
            val unitChar = parts(5).charAt(0)
            val pieceName = pieceNameFromFenChar(unitChar)

            val generalBoardAction =
              DoGeneralBoardAction(
                BuyReinforcement(pieceName, free = true),
                actionId
              )

            List(
              Protocol.DoBoardAction(
                boardIdx,
                generalBoardAction
              )
            )

          case "move" =>
            val (fromLoc, toLoc) = parseLocPair(parts(4))
            val movement = makeMovement(boardIdx, fromLoc, toLoc)
            val playerActions =
              PlayerActions(List(Movements(List(movement))), actionId)
            List(Protocol.DoBoardAction(boardIdx, playerActions))

          case "move_cyclic" =>
            val args = parts.slice(4, parts.length)
            val locs = args.map(parseLocation).toList

            val loc_pairs =
              locs.sliding(2).toList ++ List(List(locs.last, locs.head))

            val movements = loc_pairs.map { case List(fromLoc, toLoc) =>
              makeMovement(boardIdx, fromLoc, toLoc)
            }
            val playerActions =
              PlayerActions(List(Movements(movements)), actionId)
            List(Protocol.DoBoardAction(boardIdx, playerActions))

          case "attack" =>
            val (fromLoc, toLoc) = parseLocPair(parts(4))
            val attacker = curPiece(boardIdx, fromLoc)
            val targetSpec =
              game.boards(boardIdx).curState.pieces(toLoc).head.spec
            val playerActions =
              PlayerActions(List(Attack(attacker.spec, targetSpec)), actionId)
            List(Protocol.DoBoardAction(boardIdx, playerActions))

          case "blink" =>
            val blinkLoc = parseLocation(parts(4))
            val pieceSpec = curPiece(boardIdx, blinkLoc).spec
            val playerActions =
              PlayerActions(List(Blink(pieceSpec, blinkLoc)), actionId)
            List(Protocol.DoBoardAction(boardIdx, playerActions))

          case "buy" =>
            val unitChar = parts(4).charAt(0)
            val pieceName = pieceNameFromFenChar(unitChar)
            val action =
              DoGeneralBoardAction(BuyReinforcement(pieceName, false), actionId)
            List(Protocol.DoBoardAction(boardIdx, action))

          case "spawn" =>
            val unitChar = parts(4).charAt(0)
            val spawnLoc = parseLocation(parts(5))
            val pieceName = pieceNameFromFenChar(unitChar)
            val playerActions =
              PlayerActions(List(Spawn(spawnLoc, pieceName)), actionId)
            List(Protocol.DoBoardAction(boardIdx, playerActions))
        }

      case "tech" =>
        val advanceBy = parts(2).toInt
        val acquire = parts.slice(3, parts.length).map(_.toInt)
        val side = game.game.curSide
        val spellsToBuy = advanceBy + acquire.length - 1
        val buySpells = (0 until spellsToBuy)
          .map(_ => Protocol.DoGameAction(BuyExtraTechAndSpell(side)))
          .toList
        val idx = game.game.techLine.indexWhere { techState =>
          techState.level(side) == TechLocked
        }
        val advanceTech = (0 until advanceBy)
          .map(i => Protocol.DoGameAction(PerformTech(side, idx + i)))
          .toList
        val acquireTech = acquire
          .map(i => Protocol.DoGameAction(PerformTech(side, i + 2)))
          .toList
        buySpells ++ advanceTech ++ acquireTech
    }
  }

  private def curPiece(boardIdx: Int, loc: Loc): Piece = {
    if (movers(boardIdx).contains(loc)) {
      movers(boardIdx)(loc)
    } else {
      game.boards(boardIdx).curState.pieces(loc).head
    }
  }

  private def makeMovement(
      boardIdx: Int,
      fromLoc: Loc,
      toLoc: Loc
  ): Movement = {
    val board = game.boards(boardIdx).curState
    val pieces = board.pieces(fromLoc)
    if (pieces.isEmpty) {
      throw new Exception(
        s"Invalid move ${fromLoc} -> $toLoc (no pieces at $fromLoc)"
      )
    }
    val piece = pieces.head // fails for invalid moves
    val path = board.findPathForUI(
      piece,
      pathBias = List(),
      isRotationPath = false
    ) { case (loc, _) => loc == toLoc } match {
      case None => throw new Exception(s"Invalid move ${piece.loc} -> $toLoc")
      case Some((path, _)) => path
    }
    movers(boardIdx) += (toLoc -> piece)
    Movement(piece.spec, path.toVector)
  }

  private def parseLocPair(locPairStr: String): (Loc, Loc) = {
    val coords1 = locPairStr.slice(0, 2)
    val coords2 = locPairStr.slice(2, 4)
    (parseLocation(coords1), parseLocation(coords2))
  }

  private def parseLocation(locStr: String): Loc = {
    // format: a0-j9
    val x = locStr.charAt(0) - 'a'
    val y = locStr.charAt(1) - '0'
    Loc(x, y)
  }

  private def pieceNameFromFenChar(char: Char): PieceName = {
    char.toUpper match {
      case 'Z' => "zombie"
      case 'I' => "initiate"
      case 'S' => "skeleton"
      case 'P' => "serpent"
      case 'W' => "warg"
      case 'G' => "ghost"
      case 'T' => "wight"
      case 'H' => "haunt"
      case 'K' => "shrieker"
      case 'X' => "spectre"
      case 'A' => "rat"
      case 'U' => "sorcerer"
      case 'J' => "witch"
      case 'V' => "vampire"
      case 'M' => "mummy"
      case 'L' => "lich"
      case 'O' => "void"
      case 'C' => "cerberus"
      case 'R' => "wraith"
      case 'Q' => "horror"
      case 'B' => "banshee"
      case 'E' => "elemental"
      case 'Y' => "harpy"
      case 'D' => "shadowlord"
      case 'N' => "necromancer"
      case _   => throw new Exception(s"Invalid piece name: $char")
    }
  }
}

object TurnParser {
  def splitTurns(lines: List[String]): List[List[String]] = {
    var turns = List[List[String]]()
    var turn = List[String]()
    lines.foreach { line =>
      if (line.startsWith("turn")) {
        if (turn.nonEmpty) {
          turns = turns :+ turn
          turn = List()
        }
      } else if (line.nonEmpty) {
        turn = turn :+ line
      }
    }
    if (turn.nonEmpty) {
      turns = turns :+ turn
    }
    turns
  }
}
