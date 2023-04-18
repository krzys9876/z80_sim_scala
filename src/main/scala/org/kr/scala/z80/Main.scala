package org.kr.scala.z80

import org.kr.scala.z80.system._
import org.kr.scala.z80.utils.Args

import scala.jdk.CollectionConverters.ListHasAsScala
import java.nio.file.{Files, Path}
import java.time.temporal.ChronoUnit
import java.time.LocalDateTime
import scala.concurrent.ExecutionContext

object Main extends App {
  import ExecutionContext.Implicits._
  println("INIT")

  val clArgs=new Args(args)
  println(f"mode: ${clArgs.mode()}")
  println(f"memory: ${clArgs.memoryType()}")
  println(f"register: ${clArgs.registerType()}")
  println(f"steps: ${if(clArgs.steps==Long.MaxValue) "infinite" else clArgs.steps}")

  val CONTROL_PORT = PortID(0xB1)
  val DATA_PORT = PortID(0xB0)
  val MEMORY_TOP = "65529" // 65536 does not work in interactive mode, it must be a little less than that
  val MAX_STEPS = clArgs.steps

  //debugger
  implicit val debugger:Debugger=ConsoleDebugger
  // memory
  implicit val memoryHandler:MemoryHandler = clArgs.memoryType().toLowerCase match {
    case "slow" | "s" => new ImmutableMemoryHandler()
    case _ => new MutableMemoryHandler()
  }
  val memory=prepareMemory(clArgs.hexFile())
  // register
  implicit val registerHandler:RegisterHandler = clArgs.registerType().toLowerCase match {
    case "slow" | "s" => new ImmutableRegisterHandler()
    case _ => new MutableRegisterHandler()
  }
  // input keys sequence
  val input =  clArgs.mode().toLowerCase match {
    case "interactive" | "i" => prepareConsoleInput(clArgs.basicFile())
    case "batch" | "b" => prepareInputFromFile(clArgs.basicFile())
  }
  //whole system
  val interrupts=if(clArgs.interrupts()) system.CyclicInterrupt.every20ms else NoInterrupt()
  val initSystem=new Z80System(memory,registerHandler.blank,OutputFile.blank,input,0,interrupts)

  println("START")
  val startTime=LocalDateTime.now()

  val after=StateWatcher[Z80System](initSystem) >>== Z80System.run(debugger)(MAX_STEPS)

  val endTime=LocalDateTime.now()
  val seconds=ChronoUnit.MILLIS.between(startTime,endTime).toDouble/1000
  val cycles=after.get.elapsedTCycles
  val refhz=Z80System.REFERENCE_HZ
  val refseconds=cycles.toDouble/refhz.toDouble
  val speed=refseconds/seconds // assuming CPU @ 3.6468MHZ
  println(f"elapsed seconds: $seconds%1.2f")
  println(f"elapsed T cycles: $cycles")
  println(f"reference clock ${refhz.toDouble/1000000}%1.4f MHz")
  println(f"reference seconds: $refseconds%1.2f")
  println(f"relative speed: ${speed*100}%2.2f %%")
  println("END")

  private def readFile(fullFileWithPath:String):List[String]=
    Files.readAllLines(Path.of(fullFileWithPath)).asScala.toList

  private def prepareMemory(hexFile: String)(implicit memoryHandler:MemoryHandler): MemoryContents =
    (StateWatcher(memoryHandler.blank(0x10000)) >>==
      memoryHandler.loadHexLines(readFile(hexFile)) >>==
      memoryHandler.lockTo(0x2000))
      .state

  private def prepareInputFromFile(inputTextFile:String):InputFile={
    // add initial "memory top" answer to skip long-lasting memory test
    val inputLines=readTextFile(inputTextFile)
    val inputList:List[Int]=InputFile.linesList2Ints(inputLines)
    val inputPortKeys=new InputPortMultiple(inputList)
    val inputPortControl=new InputPortMultiple(List.fill(inputList.length)(1))
    InputFile.blank
      .attachPort(CONTROL_PORT,inputPortControl)
      .attachPort(DATA_PORT,inputPortKeys)
  }

  private def readTextFile(inputTextFile: String):List[String] = {
    if(inputTextFile.nonEmpty) List(MEMORY_TOP) ++ readFile(inputTextFile)
    else List()
  }

  private def prepareConsoleInput(inputTextFile:String):InputFile = prepareConsoleInput(readTextFile(inputTextFile))
  private def prepareConsoleInput(initialLines:List[String]=List()):InputFile={
    val chars=if(initialLines.nonEmpty) initialLines.foldLeft("")((fullString,line)=>fullString+line+"\r") else ""
    val consolePort=new InputPortConsole(chars.toCharArray)
    val controlPort=new InputPortControlConsole(consolePort)
    InputFile.blank
      .attachPort(CONTROL_PORT,controlPort)
      .attachPort(DATA_PORT,consolePort)
  }
}
