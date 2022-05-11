package org.kr.scala.z80

import org.kr.scala.z80.system.{CharFormatter, ConsoleDebugger, Debugger, DummyDebugger, InputController, InputFile, InputPortMultiple, MemoryController, OutputController, OutputFormatter, Outputter, PrintOutputter, RegisterController, Z80System, Z80SystemController}

import scala.jdk.CollectionConverters.ListHasAsScala
import java.nio.file.{Files, Path}
import java.time.temporal.ChronoUnit
import java.time.LocalDateTime

object Main extends App {

  if(args.length!=2) {
    println("Incorrect input parameters: hex_file input_file")
    System.exit(1)
  }

  println("START")
  val startTime=LocalDateTime.now()

  val CONTROL_PORT=0xB1
  val DATA_PORT=0xB0
  val MEMORY_TOP="65536"
  val MAX_STEPS=450000L
  //val MAX_STEPS=Long.MaxValue
  // memory
  val memory=prepareMemory(args(0))
  // input keys sequence
  val input=prepareInput(args(1))
  //whole system
  val initSystem=Z80SystemController(new Z80System(memory,RegisterController.blank,OutputController.blank,input))

  implicit val debugger:Debugger=ConsoleDebugger
  val after=initSystem >>= Z80SystemController.run(debugger)(MAX_STEPS)

  //printOutput()

  val endTime=LocalDateTime.now()
  val mseconds=ChronoUnit.MILLIS.between(startTime,endTime)
  println(f"elapsed miliseconds: $mseconds")
  println("END")

  //1. initial version: ~29 seconds
  //2. limit opcode lists in handlers only to those handled by handler: ~16-20 seconds
  // pros: lists as shorter, maintains compile time type checking
  // cons: still uses list lookup
  //3. runtime type conversion from OpCodes.list to type required by handler - same or little slower than limiting lists
  // pros: simpifies code
  // cons: users runtime type casting which may resuly in runtime exception, not compile time exception
  //4. as 3. but simplify Z80System.handle (do not lookup twice) ~ 14 sec. - bes option so far

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