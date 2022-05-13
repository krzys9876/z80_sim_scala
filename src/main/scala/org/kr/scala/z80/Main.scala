package org.kr.scala.z80

import org.kr.scala.z80.system.{CharFormatter, ConsoleDebugger, Debugger, InputController, InputFile,
  InputPortMultiple, MemoryController, OutputController, OutputFormatter, Outputter, PrintOutputter,
  RegisterController, Z80System, Z80SystemController}

import scala.jdk.CollectionConverters.ListHasAsScala
import java.nio.file.{Files, Path}
import java.time.temporal.ChronoUnit
import java.time.LocalDateTime

object Main extends App {

  if(args.length<2 || args.length>3) {
    println("Incorrect input parameters: hex_file input_file [steps]")
    System.exit(1)
  }

  println("INIT")

  val CONTROL_PORT=0xB1
  val DATA_PORT=0xB0
  val MEMORY_TOP="65536"
  val MAX_STEPS=if(args.length==3) args(2).toLong else Long.MaxValue
  // memory
  val memory=prepareMemory(args(0))
  // input keys sequence
  val input=prepareInput(args(1))
  //whole system
  val initSystem=new Z80System(memory,RegisterController.blank,OutputController.blank,input,0)

  println("START")
  val startTime=LocalDateTime.now()

  implicit val debugger:Debugger=ConsoleDebugger
  val after=Z80SystemController(initSystem) >>= Z80SystemController.run(debugger)(MAX_STEPS)

  val endTime=LocalDateTime.now()
  val seconds=ChronoUnit.MILLIS.between(startTime,endTime).toDouble/1000
  val cycles=after.get.elapsedTCycles
  val refmhz=3686400
  val refseconds=cycles.toDouble/refmhz.toDouble
  val speed=refseconds/seconds // assuming CPU @ 3.6468MHZ
  println(f"elapsed seconds: $seconds%1.2f")
  println(f"elapsed T cycles: $cycles")
  println(f"reference clock ${refmhz.toDouble/1000000}%1.4f MHz")
  println(f"reference seconds: $refseconds%1.2f")
  println(f"relative speed: ${speed*100}%2.2f %%")
  println("END")

  //1. initial version: ~29 seconds
  //2. limit opcode lists in handlers only to those handled by handler: ~16-20 seconds
  // pros: lists as shorter, maintains compile time type checking
  // cons: still uses list lookup
  //3. runtime type conversion from OpCodes.list to type required by handler - same or little slower than limiting lists
  // pros: simpifies code
  // cons: users runtime type casting which may resuly in runtime exception, not compile time exception
  //4. as 3. but simplify Z80System.handle (do not lookup twice) ~ 14 sec. - best option so far
  //5. reduce all list lookups to one per step ~4-5 seconds
  //6. replace register map with vals ~3-4 seconds


  private def readFile(fullFileWithPath:String):List[String]=
    Files.readAllLines(Path.of(fullFileWithPath)).asScala.toList

  private def prepareMemory(hexFile:String):MemoryController={
    val hexLines=readFile(hexFile)
    MemoryController.blank(0x10000) >>= MemoryController.loadHexLines(hexLines) >>= MemoryController.lockTo(0x4000)
  }

  private def prepareInput(inputFile:String):InputController={
    // add initial "memory top" answer to skip long-lasting memory test
    val inputLines=List(MEMORY_TOP) ++ readFile(inputFile)
    val inputList:List[Int]=InputFile.linesList2Ints(inputLines)
    val inputPortKeys=new InputPortMultiple(inputList)
    val inputPortControl=new InputPortMultiple(List.fill(inputList.length)(1))
    InputController.blank >>=
      InputController.attachPort(CONTROL_PORT,inputPortControl) >>=
      InputController.attachPort(DATA_PORT,inputPortKeys)
  }

  private def printOutput():Unit={
    implicit val outputFormatter:OutputFormatter=CharFormatter
    implicit val outputter:Outputter=PrintOutputter
    after.get.outputController.get.show(DATA_PORT)
  }
}