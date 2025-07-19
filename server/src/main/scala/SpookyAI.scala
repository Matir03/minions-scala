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
      sendToEngine("umi")
      waitForResponse("umiok")
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
    Log.log(s"Sending to engine: $command")
    engineInputQueue.put(command)
  }

  def receiveLineFromEngine(): String = {
    var line: String = ""
    line = engineOutputQueue.take()
    Log.log(s"Received from engine: $line")
    line
  }

  def waitForResponse(expected: String): Unit = {
    var line: String = null
    while ({ line = receiveLineFromEngine(); line != expected }) {
      // Keep reading until we get the expected response
    }
    if (line != expected) {
      throw new Exception(s"Expected $expected but got $line")
    }
  }

  def getBestMove(position: String): Option[List[String]] = {
    sendToEngine(s"position $position")
    sendToEngine("play movetime 1000") // Think for 1 second and get full turn

    var actions = List[String]()
    var line: String = null

    while ({ line = receiveLineFromEngine(); line != "endturn" && line != null }) {
      if (line.startsWith("action ")) {
        actions = actions :+ line
      }
    }

    if (actions.nonEmpty) {
      Some(actions)
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
      getBestMove(position).foreach { actions =>
        // Convert each UCI action to game action and send them
        actions.foreach { action =>
          convertUCIMoveToAction(action).foreach { gameAction =>
            out ! gameAction
          }
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
            val playerActions = PlayerActions(List(Movements(List(movement))), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "attack" if parts.length >= 6 =>
            val attackerSpec = StartedTurnWithID(-1) // TODO: Find actual piece ID
            val targetSpec = StartedTurnWithID(-1) // TODO: Find actual piece ID
            val playerActions = PlayerActions(List(Attack(attackerSpec, targetSpec)), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "spawn" if parts.length >= 6 =>
            val spawnLoc = parseLocation(parts(4))
            val unitName = parts(5)
            val pieceName: PieceName = unitName // PieceName is just a String type alias
            val playerActions = PlayerActions(List(Spawn(spawnLoc, pieceName)), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "blink" if parts.length >= 5 =>
            val blinkLoc = parseLocation(parts(4))
            val pieceSpec = StartedTurnWithID(-1) // TODO: Find actual piece ID
            val playerActions = PlayerActions(List(Blink(pieceSpec, blinkLoc)), actionId)
            Some(Protocol.DoBoardAction(boardIdx, playerActions))

          case "cast" if parts.length >= 6 =>
            val targetLoc = parseLocation(parts(5))
            val spellId = 0 // TODO: Find actual spell ID
            val targets = SpellOrAbilityTargets.singleLoc(targetLoc) // Use the correct type and factory method
            val playerActions = PlayerActions(List(PlaySpell(spellId, targets)), actionId)
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
