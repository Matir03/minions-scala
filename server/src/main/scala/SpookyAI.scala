package minionsgame.server

import scala.util.{Success, Failure}
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import java.io.{
  BufferedReader,
  BufferedWriter,
  InputStreamReader,
  OutputStreamWriter
}
import scala.sys.process._
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import akka.actor.{Actor, ActorRef, Props}
import play.api.libs.json._

import minionsgame.core._
import RichImplicits._

private class SpookyAI(out: ActorRef, game: GameState, enginePath: String)
    extends Actor {
  var name = "Spooky"
  var side = Some(S1)
  val aiRand = Rand(RandUtils.sha256Long(game.randSeed + "#spooky"))
  var nextActionIdSuffix: Int = 0

  // UCI engine process
  private var engineProcess: Option[Process] = None
  private val engineInputQueue: BlockingQueue[String] =
    new LinkedBlockingQueue[String]()
  private val engineOutputQueue: BlockingQueue[String] =
    new LinkedBlockingQueue[String]()

  private val movers: Array[Map[Loc, Piece]] =
    Array.fill(game.numBoards)(Map())

  def makeActionId(): String = {
    nextActionIdSuffix = nextActionIdSuffix + 1
    name + nextActionIdSuffix.toString
  }

  def chat(message: String) = {
    out ! Protocol.Chat(name, side, true, message)
  }

  def startEngine(): Unit = {
    val processIO = new ProcessIO(
      // Handle process input
      in => {
        val writer = new BufferedWriter(new OutputStreamWriter(in))
        try {
          while (true) {
            val cmd = engineInputQueue.take()
            writer.write(cmd + "\n")
            writer.flush()
          }
        } catch {
          case _: InterruptedException =>
            writer.close()
        }
      },
      // Handle process output
      out => {
        val reader = new BufferedReader(new InputStreamReader(out))
        try {
          var line: String = null
          while ({ line = reader.readLine(); line != null }) {
            engineOutputQueue.put(line)
          }
        } finally {
          reader.close()
        }
      },
      // Handle process error
      err => {
        val reader = new BufferedReader(new InputStreamReader(err))
        try {
          var line: String = null
          while ({ line = reader.readLine(); line != null }) {
            val msg = s"spooky err> $line"
            Log.log(msg)
            chat(msg)
          }
        } finally {
          reader.close()
        }
      },
      daemonizeThreads = true
    )

    val pb = Process(enginePath)
    engineProcess = Some(pb.run(processIO))

    // Initialize UCI engine
    sendToEngine("umi")
    waitForResponse("umiok")
    sendToEngine("isready")
    val _ = waitForResponse("readyok")
    sendToEngine("config " + getConfig(game))
  }

  def stopEngine(): Unit = {
    try {
      engineProcess.foreach(_.destroy())
    } finally {
      engineProcess = None
    }
  }

  def sendToEngine(command: String): Unit = {
    Log.log(s"spooky < $command")
    chat(s"spooky < $command")
    engineInputQueue.put(command)
  }

  def receiveLineFromEngine(): String = {
    var line: String = ""
    line = engineOutputQueue.take()
    Log.log(s"spooky > $line")
    chat(s"spooky > $line")
    line
  }

  def waitForResponse(expected: String): List[String] = {
    var lines: List[String] = List()
    while (true) {
      val line = receiveLineFromEngine()
      lines = lines :+ line
      if (line.startsWith(expected)) {
        return lines
      }
    }
    lines
  }

  def getBestMove(): List[String] = {
    sendToEngine("position fen " + convertGameStateToFEN())

    sendToEngine("play nodes 1")

    val _ = waitForResponse("turn")

    waitForResponse("endturn")
  }

  override def preStart(): Unit = {
    startEngine()
  }

  override def postStop(): Unit = {
    stopEngine()
  }

  override def receive: Receive = {
    case Protocol.ReportTimeLeft(_) => ()

    case Protocol.ReportNewTurn(S1) if game.game.winner.isEmpty =>
      for (i <- 0 until game.numBoards) {
        movers(i) = Map()
      }

      // Get best move from engine
      getBestMove().foreach { line =>
        // Convert each UCI action to game action and send them
        val gameActions = convertUCIMoveToActions(line)
        gameActions.foreach { action =>
          out ! action
        }
      }

    case _ => ()
  }

  private def convertGameStateToFEN(): String = {
    /// fen <money> <board_points> <tech_state> <board_fen> ... <board_fen> <side_to_move> <turn_num>

    def encodeTechState(techStates: Array[TechState]): String = {
      // For each tech, compute state
      val team0Spells = techStates
        .slice(1, techStates.length)
        .map { techState =>
          techState.level.apply(S0).toString.charAt(0)
        }
        .mkString("")
      val team1Spells = techStates
        .slice(1, techStates.length)
        .map { techState =>
          techState.level.apply(S1).toString.charAt(0)
        }
        .mkString("")
      s"$team0Spells|$team1Spells"
    }

    def encodeBoardPosition(board: Board): String = {
      val boardState = board.curState()
      val position = (0 to 9)
        .map { y =>
          var emptyCount = 0
          val rowStr = new StringBuilder()
          (0 to 9).foreach { x =>
            boardState.pieces(Loc(x, y)) match {
              case Nil => emptyCount += 1
              case piece :: Nil =>
                if (emptyCount > 0) {
                  rowStr.append(emptyCount.toString)
                  emptyCount = 0
                }
                rowStr.append(fenChar(piece.baseStats.name, piece.side))
              case _ =>
                throw new Exception("Unexpected multiple pieces at location")
            }
          }
          if (emptyCount > 0) {
            if (emptyCount == 10) {
              rowStr.append("0")
            } else {
              rowStr.append(emptyCount.toString)
            }
          }
          rowStr.toString()
        }
        .mkString("/")

      val r0 = boardState
        .reinforcements(S0)
        .map { case (p, n) =>
          (0 until n).map(_ => fenChar(p, S0)).mkString("")
        }
        .mkString("")

      val r1 = boardState
        .reinforcements(S1)
        .map { case (p, n) =>
          (0 until n).map(_ => fenChar(p, S1)).mkString("")
        }
        .mkString("")

      s"n|$r0|$r1|||$position"
    }

    val money = s"${game.game.souls(S0)}|${game.game.souls(S1)}"
    val boardPoints = s"${game.game.wins(S0)}|${game.game.wins(S1)}"
    val techState = encodeTechState(game.game.techLine)
    // Encode each board's position and spells
    val boards = game.boards
      .map { board => encodeBoardPosition(board) }
      .mkString(" ")
    val side = game.game.curSide.int
    val turnNum = game.game.turnNumber

    s"$money $boardPoints $techState $boards $side $turnNum"
  }

  private def fenChar(pieceName: PieceName, side: Side): Char = {
    val char = pieceName match {
      case "zombie"              => 'Z'
      case "initiate"            => 'I'
      case "skeleton"            => 'S'
      case "serpent"             => 'P'
      case "warg"                => 'W'
      case "ghost"               => 'G'
      case "wight"               => 'T'
      case "haunt"               => 'H'
      case "shrieker"            => 'K'
      case "spectre"             => 'X'
      case "bone rat"            => 'A'
      case "sorcerer"            => 'U'
      case "witch"               => 'J'
      case "vampire"             => 'V'
      case "mummy"               => 'M'
      case "lich"                => 'L'
      case "void"                => 'O'
      case "cerberus"            => 'C'
      case "wraith"              => 'R'
      case "horror"              => 'Q'
      case "banshee"             => 'B'
      case "elemental"           => 'E'
      case "harpy"               => 'Y'
      case "shadowlord"          => 'D'
      case "necromancer"         => 'N'
      case "arcane_necromancer"  => 'N'
      case "ranged_necromancer"  => 'N'
      case "mounted_necromancer" => 'N'
      case "deadly_necromancer"  => 'N'
      case "battle_necromancer"  => 'N'
      case "zombie_necromancer"  => 'N'
      case "mana_necromancer"    => 'N'
      case "terrain_necromancer" => 'N'
    }

    if (side == S1) {
      char.toLower
    } else {
      char
    }
  }

  private def mapNameToFen(mapName: String): String = {
    mapName.replaceAll(" ", "")
  }

  private def getConfig(gameState: GameState): String = {
    val pointsToWin = gameState.game.targetNumWins
    val maps = gameState.boardNames.map(mapNameToFen).mkString(",")
    val techs =
      gameState.game.techLine
        .slice(1, gameState.game.techLine.length)
        .map(t =>
          t.tech match {
            case PieceTech(pieceName) => fenChar(pieceName, S0)
            case Copycat              => '1'
            case TechSeller           => '2'
            case Metamagic            => '3'
          }
        )
        .mkString(",")
    s"$pointsToWin $maps $techs"
  }

  private def convertUCIMoveToActions(
      uciMove: String
  ): List[Protocol.Query] = {
    val parts = uciMove.split(" ")

    if (parts(0) == "endturn") {
      return (0 until game.numBoards)
        .map(i => Protocol.DoGameAction(SetBoardDone(i, true)))
        .toList
    }

    val actionType = parts(1)
    val actionId = makeActionId()

    actionType match {
      case "b_attack" =>
        val boardIdx = parts(2).toInt
        val boardActionType = parts(3)

        boardActionType match {
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
        }

      case "b_spawn" =>
        val boardIdx = parts(2).toInt
        val boardActionType = parts(3)

        boardActionType match {
          case "buy" =>
            val unitChar = parts(4).charAt(0)
            val pieceName = pieceNameFromFenChar(unitChar.toUpper)
            val action =
              DoGeneralBoardAction(BuyReinforcement(pieceName, false), actionId)
            List(Protocol.DoBoardAction(boardIdx, action))

          case "spawn" =>
            val unitChar = parts(4).charAt(0)
            val spawnLoc = parseLocation(parts(5))
            val pieceName = pieceNameFromFenChar(unitChar.toUpper)
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
          .map(i => Protocol.DoGameAction(PerformTech(side, i + 1)))
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
    val piece = board.pieces(fromLoc).head
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
    char match {
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

object SpookyAI {

  def props(out: ActorRef, game: GameState, enginePath: String): Props =
    Props(new SpookyAI(out, game, enginePath))
}
