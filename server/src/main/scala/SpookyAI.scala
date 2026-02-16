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
    sendToEngine("position fen " + game.convertGameStateToFEN())

    sendToEngine("play")

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

  private def getConfig(gameState: GameState): String = {
    val pointsToWin = gameState.game.targetNumWins
    val maps = gameState.boardNames.map(mapNameToFen).mkString(",")
    val techs =
      gameState.game.techLine
        .slice(1, gameState.game.techLine.length)
        .map(t =>
          t.tech match {
            case PieceTech(pieceName) => game.fenChar(pieceName, S0)
            case Copycat              => '1'
            case TechSeller           => '2'
            case Metamagic            => '3'
          }
        )
        .mkString(",")
    s"$pointsToWin $maps $techs"
  }

  private def mapNameToFen(mapName: String): String = {
    mapName.replaceAll(" ", "")
  }
}

object SpookyAI {
  def props(out: ActorRef, game: GameState, enginePath: String): Props =
    Props(new SpookyAI(out, game, enginePath))
}
