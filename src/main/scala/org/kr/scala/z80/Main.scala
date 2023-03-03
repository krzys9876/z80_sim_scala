package org.kr.scala.z80

import org.kr.scala.z80.system.{ConsoleDebugger, Debugger, InputFile, InputPortConsole, InputPortControlConsole, InputPortMultiple, Memory, NoInterrupt, OutputFile, PortID, Register, StateWatcher, Z80System}
import org.kr.scala.z80.utils.Args

import scala.jdk.CollectionConverters.ListHasAsScala
import java.nio.file.{Files, Path}
import java.time.temporal.ChronoUnit
import java.time.LocalDateTime
import scala.concurrent.ExecutionContext

object Main extends App {

  import ExecutionContext.Implicits._

  val clArgs=new Args(args)
  println("INIT")

  println(f"mode: ${clArgs.mode()}")

  val CONTROL_PORT = PortID(0xB1)
  val DATA_PORT = PortID(0xB0)
  val MEMORY_TOP = "65529" // 65536 does not work in interactive mode, it must be a little less than that
  val MAX_STEPS = clArgs.steps

  //debugger
  implicit val debugger:Debugger=ConsoleDebugger
  // memory
  val memory=prepareMemory(clArgs.hexFile())
  // input keys sequence
  val input =  clArgs.mode().toLowerCase match {
    case "interactive" | "i" => prepareConsoleInput(clArgs.basicFile())
    case "batch" | "b" => prepareInputFromFile(clArgs.basicFile())
  }
  //whole system
  val interrupts=if(clArgs.interrupts()) system.CyclicInterrupt.every20ms else NoInterrupt()
  val ioMappingMode=if(clArgs.ioPorts16Bit()) Z80System.use16BitIOPorts else Z80System.use8BitIOPorts
  val initSystem=new Z80System(memory,Register.blank,OutputFile.blank,input,0, ioMappingMode,interrupts)

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

  //1. initial version: ~29 seconds
  //2. limit opcode lists in handlers only to those handled by handler: ~16-20 seconds
  // pros: lists as shorter, maintains compile time type checking
  // cons: still uses list lookup
  //3. runtime type conversion from OpCodes.list to type required by handler - same or little slower than limiting lists
  // pros: simplifies code
  // cons: uses runtime type casting which may result in runtime exception, not compile time exception
  //4. as 3. but simplify Z80System.handle (do not lookup twice) ~ 14 sec. - best option so far
  //5. reduce all list lookups to one per step ~4-5 seconds
  //6. replace register map with vals ~3-4 seconds
  //7. remove unnecessary state watchers ~10-15%

  private def readFile(fullFileWithPath:String):List[String]=
    Files.readAllLines(Path.of(fullFileWithPath)).asScala.toList

  private def prepareMemory(hexFile:String):Memory={
    val hexLines=readFile(hexFile)
    Memory.blank(0x10000)
      .loadHexLines(hexLines)
      .lockTo(0x2000)
  }

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
