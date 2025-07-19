package minionsgame.server

import org.scalatest.FunSuite
import scala.util.{Try, Success, Failure}
import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import scala.sys.process._
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

class SpookyAIIntegrationTest extends FunSuite {

  test("SpookyAI should be able to communicate with the Rust engine") {
    val enginePath = "../spooky/target/release/spooky"

    // Check if the engine exists
    val engineFile = new java.io.File(enginePath)
    if (!engineFile.exists()) {
      pending // Skip test if engine doesn't exist
    }

    var engineProcess: Option[Process] = None
    val engineInputQueue: BlockingQueue[String] = new LinkedBlockingQueue[String]()
    val engineOutputQueue: BlockingQueue[String] = new LinkedBlockingQueue[String]()

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
              println(s"Engine error: $line")
            }
          } finally {
            reader.close()
          }
        },
        daemonizeThreads = true
      )

      val pb = Process(enginePath)
      engineProcess = Some(pb.run(processIO))

      def sendToEngine(command: String): Unit = {
        engineInputQueue.put(command)
      }

      def waitForResponse(expected: String): Boolean = {
        var line: String = null
        var attempts = 0
        val maxAttempts = 10

        while (attempts < maxAttempts) {
          try {
            line = engineOutputQueue.take()
            if (line == expected) {
              return true
            }
            attempts += 1
          } catch {
            case _: InterruptedException =>
              attempts += 1
          }
        }
        false
      }

      // Test UMI initialization
      sendToEngine("umi")
      assert(waitForResponse("umiok"))

      // Test ready check
      sendToEngine("isready")
      waitForResponse("readyok") shouldBe true

      // Test position setting
      sendToEngine("position startpos")

      // Test display command
      sendToEngine("display")

      // Test quit
      sendToEngine("quit")

    } finally {
      try {
        engineProcess.foreach(_.destroy())
      } catch {
        case _: Exception => // Ignore cleanup errors
      }
    }
  }
}