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

  def getBestTurn(): List[String] = {
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
    case Protocol.ReportTurnStart(S1) if game.game.winner.isEmpty =>
      val turn = getBestTurn()
      val turnParser = new TurnParser(game, () => makeActionId())
      turn.foreach { move =>
        turnParser.convertUMIMoveToActions(move).foreach { action =>
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

      val rein0 = (boardState
        .reinforcements(S0)
        .map { case (p, n) =>
          (0 until n).map(_ => fenChar(p, S0)).mkString("")
        } ++ boardState.allowedFreeBuyPieces(S0).map(fenChar(_, S0)))
        .mkString("")

      val rein1 = (boardState
        .reinforcements(S1)
        .map { case (p, n) =>
          (0 until n).map(_ => fenChar(p, S1)).mkString("")
        } ++ boardState.allowedFreeBuyPieces(S1).map(fenChar(_, S1)))
        .mkString("")

      val reset = boardState.resetState match {
        case FirstTurn => "f"
        case Normal    => "n"
        case Reset1    => "1"
        case Reset2    => "2"
        case JustEnded =>
          throw new Exception("JustEnded should never be sent to the AI")
      }

      s"$reset|$rein0|$rein1|||$position"
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

  private def mapNameToFen(mapName: String): String = {
    mapName.replaceAll(" ", "")
  }

  private def getConfig(gameState: GameState): String = {
    val pointsToWin = gameState.game.targetNumWins
    val maps = gameState.boardNames.map(mapNameToFen).mkString(",")
    val techs =
      gameState.game.techLine
        .slice(2, gameState.game.techLine.length)
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

}

object SpookyAI {
  def props(out: ActorRef, game: GameState, enginePath: String): Props =
    Props(new SpookyAI(out, game, enginePath))
}
