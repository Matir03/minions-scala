package minionsgame.server

import scala.util.{Try,Success,Failure}
import scala.concurrent.Future
import java.util.concurrent.atomic.AtomicInteger
import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import scala.sys.process._
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import akka.actor.{Actor, ActorRef, Props}
import play.api.libs.json._

import minionsgame.core._
import RichImplicits._

private class SpookyAI(out: ActorRef, game: GameState, enginePath: String) extends Actor {
  var name = "Spooky"
  var side = Some(S1)
  val aiRand = Rand(RandUtils.sha256Long(game.randSeed + "#spooky"))
  var nextActionIdSuffix: Int = 0

  // UCI engine process
  private var engineProcess: Option[Process] = None
  private val engineInputQueue: BlockingQueue[String] = new LinkedBlockingQueue[String]()
  private val engineOutputQueue: BlockingQueue[String] = new LinkedBlockingQueue[String]()

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
      sendToEngine("uci")
      waitForResponse("uciok")
      sendToEngine("isready")
      waitForResponse("readyok")
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
    engineInputQueue.put(command)
  }

  def waitForResponse(expected: String): Unit = {
    var line: String = null
    while ({ line = engineOutputQueue.take(); line != expected }) {
      // Keep reading until we get the expected response
    }
    if (line != expected) {
      throw new Exception(s"Expected $expected but got $line")
    }
  }

  def getBestMove(position: String): Option[String] = {
    sendToEngine(s"position $position")
    sendToEngine("go movetime 1000") // Think for 1 second

    var line: String = null
    while ({ line = engineOutputQueue.take(); !line.startsWith("bestmove") }) {
      // Keep reading until we get the bestmove response
    }

    if (line.startsWith("bestmove")) {
      Some(line.split(" ")(1))
    } else None
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
      // Convert current game state to UCI format
      val position = convertGameStateToUCI(game)

      // Get best move from engine
      getBestMove(position).foreach { move =>
        // Convert UCI move to game action
        convertUCIMoveToAction(move).foreach { action =>
          out ! action
        }
      }

    case _ => ()
  }

  private def convertGameStateToUCI(gameState: GameState): String = {
    // Format: fen [side] [num_boards] [board1_pos] [board1_spells] [board2_pos] [board2_spells] ... [boardn_pos] [techline_spells] [team0_money] [team1_money]

    def encodeBoardPosition(board: Board): String = {
      // Encode each rank from 9 to 0
      val ranks = (9 to 0 by -1).map { y =>
        // For each rank, encode pieces from 0 to 9
        val row = (0 to 9).map { x =>
          val loc = Loc(x, y)
          board.curState().pieces.apply(loc) match {
            case Nil => "."  // Empty square
            case (piece :: Nil) => piece.baseStats.shortDisplayName // Use piece ID as representation
            case _ => throw new Exception("Unexpected multiple pieces at location")
          }
        }.mkString
        row
      }.mkString("/")
      ranks
    }

    def encodeBoardSpells(board: Board, side: Side): String = {
      val state = board.curState()
      val sideSpells = state.spellsInHand.apply(side)
      val oppSpells = state.spellsInHand.apply(side.opp)

      s"${sideSpells.size} ${oppSpells.size} ${sideSpells.map(id => gameState.spellMap(id)).mkString(" ")}"
    }

    def encodeTechlineSpells(): String = {
      // For each tech (1-24), count spells for each team
      val game = gameState.game
      val techLine = game.techLine;
      val team0Spells = techLine.map { techState =>
        techState.level.apply(S0).toInt.toString
      }.mkString(" ")
      val team1Spells = techLine.map { techState =>
        techState.level.apply(S1).toInt.toString
      }.mkString(" ")
      s"$team0Spells $team1Spells"
    }

    val side = gameState.game.curSide.int
    val numBoards = gameState.numBoards

    // Encode each board's position and spells
    val boardsEncoding = gameState.boards.map { board =>
      s"${encodeBoardPosition(board)} ${encodeBoardSpells(board, gameState.game.curSide)}"
    }.mkString(" ")

    val techlineSpells = encodeTechlineSpells()
    val team0Money = gameState.game.souls(S0)
    val team1Money = gameState.game.souls(S1)

    s"fen $side $numBoards $boardsEncoding $techlineSpells $team0Money $team1Money"
  }

  private def convertUCIMoveToAction(uciMove: String): Option[Protocol.Query] = {
    // TODO: Implement conversion of UCI move to game action
    // This will need to parse the UCI move and convert it to appropriate
    // Protocol.DoBoardAction or Protocol.DoGameAction
    if (uciMove.startsWith("move")) {
      val boardIdx = uciMove.split(" ")(1).toInt
      // val action = uciMove.split(" ")(2)
      val actionId = makeActionId()
      Some(Protocol.DoBoardAction(boardIdx, PlayerActions(Nil, actionId)))
    } else {
      None
    }
  }
}

object SpookyAI {
  def props(out: ActorRef, game: GameState, enginePath: String): Props =
    Props(new SpookyAI(out, game, enginePath))
}
