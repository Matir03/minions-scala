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

  def makeActionId(): String = {
    nextActionIdSuffix = nextActionIdSuffix + 1
    name + nextActionIdSuffix.toString
  }

  def chat(message: String) = {
    out ! Protocol.Chat(name, side, true, message)
  }

  def startEngine(): Unit = {
    try {
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
              Log.log(s"UCI engine error: $line")
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
      // sendToEngine("position config " + getConfig(gameState))
    } catch {
      case e: Exception =>
        chat(s"Failed to start engine: ${e.getMessage}")
        stopEngine()
    }
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

  def getBestMove(gameState: GameState): List[String] = {
    sendToEngine("position fen " + convertGameStateToFEN(gameState))

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
      // Get best move from engine
      getBestMove(game).foreach { line =>
        // Convert each UCI action to game action and send them
        val gameAction = convertUCIMoveToAction(line)
        out ! gameAction
      }

    case _ => ()
  }

  private def convertGameStateToFEN(gameState: GameState): String = {
    /// fen <money> <board_points> <tech_state> <board_fen> ... <board_fen> <side_to_move> <turn_num>

    def encodeTechState(techStates: Array[TechState]): String = {
      // For each tech, compute state
      val team0Spells = techStates
        .slice(2, techStates.length)
        .map { techState =>
          techState.level.apply(S0).toString.charAt(0)
        }
        .mkString("")
      val team1Spells = techStates
        .slice(2, techStates.length)
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

    val money = s"${gameState.game.souls(S0)}|${gameState.game.souls(S1)}"
    val boardPoints = s"${gameState.game.wins(S0)}|${gameState.game.wins(S1)}"
    val techState = encodeTechState(gameState.game.techLine)
    // Encode each board's position and spells
    val boards = gameState.boards
      .map { board => encodeBoardPosition(board) }
      .mkString(" ")
    val side = gameState.game.curSide.int
    val turnNum = gameState.game.turnNumber

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
      case "rat"                 => 'A'
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

  // private def getConfig(gameState: GameState): String = {
  //   val config = gameState.game.config
  //   s"${config.numBoards}"
  // }

  private def convertUCIMoveToAction(
      uciMove: String
  ): Option[Protocol.Query] = {
    // UMI move format: "action <actiontype> [actionparams...]"
    // Examples:
    // "action boardaction 0 move 1,2 3,4" - move piece from (1,2) to (3,4) on board 0
    // "action boardaction 0 attack 1,2 3,4" - attack from (1,2) to (3,4) on board 0
    // "action boardaction 0 spawn 3,4 Necromancer" - spawn Necromancer at (3,4) on board 0
    // "action buyspell Fireball" - buy spell
    // "action advancetech 2" - advance tech by 2
    // "action acquiretech 5" - acquire tech at index 5

    val parts = uciMove.split(" ")
    if (parts.length < 2 || parts(0) != "action") {
      return None
    }

    val actionType = parts(1)
    val actionId = makeActionId()

    actionType match {
      case "boardaction" if parts.length >= 4 =>
        val boardIdx = parts(2).toInt
        val boardActionType = parts(3)

        boardActionType match {
          case "move" if parts.length >= 6 =>
            val toLoc = parseLocation(parts(5))
            val pieceSpec = StartedTurnWithID(-1) // TODO: Find actual piece ID
            val movement = Movement(pieceSpec, Vector(toLoc))
            val playerActions =
              PlayerActions(List(Movements(List(movement))), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "attack" if parts.length >= 6 =>
            val attackerSpec = StartedTurnWithID(
              -1
            ) // TODO: Find actual piece ID
            val targetSpec = StartedTurnWithID(-1) // TODO: Find actual piece ID
            val playerActions =
              PlayerActions(List(Attack(attackerSpec, targetSpec)), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "spawn" if parts.length >= 6 =>
            val spawnLoc = parseLocation(parts(4))
            val unitName = parts(5)
            val pieceName: PieceName =
              unitName // PieceName is just a String type alias
            val playerActions =
              PlayerActions(List(Spawn(spawnLoc, pieceName)), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "blink" if parts.length >= 5 =>
            val blinkLoc = parseLocation(parts(4))
            val pieceSpec = StartedTurnWithID(-1) // TODO: Find actual piece ID
            val playerActions =
              PlayerActions(List(Blink(pieceSpec, blinkLoc)), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "cast" if parts.length >= 6 =>
            val targetLoc = parseLocation(parts(5))
            val spellId = 0 // TODO: Find actual spell ID
            val targets = SpellOrAbilityTargets.singleLoc(
              targetLoc
            ) // Use the correct type and factory method
            val playerActions =
              PlayerActions(List(PlaySpell(spellId, targets)), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "endphase" =>
            // End the current phase - this might be handled differently
            None

          case _ =>
            None
        }

      case "buyspell" if parts.length >= 3 =>
        // This would be a game action, but the current structure doesn't support it directly
        // For now, return None - this needs to be handled at a higher level
        None

      case "advancetech" if parts.length >= 3 =>
        // This would be a game action
        None

      case "acquiretech" if parts.length >= 3 =>
        // This would be a game action
        None

      case "givespell" if parts.length >= 4 =>
        // This would be a game action
        None

      case _ =>
        None
    }
  }

  private def parseLocation(locStr: String): Loc = {
    val coords = locStr.split(",")
    if (coords.length == 2) {
      Loc(coords(0).toInt, coords(1).toInt)
    } else {
      Loc(0, 0) // Default fallback
    }
  }
}

object SpookyAI {
  def props(out: ActorRef, game: GameState, enginePath: String): Props =
    Props(new SpookyAI(out, game, enginePath))
}
